; Copyright (C) 2022 Marius Ghita
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(import openssl
        http-client
        intarweb
        uri-common
        (chicken process-context)
        (chicken port)
        (chicken file)
        (chicken io)
        json
        srfi-133
        message-digest-item ; message-digest-utils egg
        sha256-primitive ; sha2 egg
        iterators
        simple-loops
        srfi-69
        srfi-135)

(define storage_zone_name (get-environment-variable "BUNNY_STORAGE_ZONE_NAME"))
(define storage_access_key (get-environment-variable "BUNNY_STORAGE_ACCESS_KEY"))
(define pullzone_id (get-environment-variable "BUNNY_PULLZONE_ID"))
(define pullzone_access_key (get-environment-variable "BUNNY_PULLZONE_ACCESS_KEY"))

(when (or (eq? #f storage_zone_name) (eq? #f storage_access_key) (eq? #f pullzone_id))
  (display "BUNNY_STORAGE_ZONE_NAME, BUNNY_STORAGE_ACCESS_KEY, BUNNY_PULLZONE_ID, BUNNY_PULLZONE_ACCESS_KEY environment variables must be defined")
  (newline)
  (exit 1))


(define (vm_extractor x)
  (vector-fold
    (lambda (tail element)
      (cond
        ((equal? "ObjectName" (car element)) (alist-update! 'ObjectName (cdr element) tail))
        ((equal? "IsDirectory" (car element)) (alist-update! 'IsDirectory (cdr element) tail))
        ((equal? "Checksum" (car element)) (alist-update! 'Checksum (cdr element) tail))
        (else tail)))
    '()
    x))

(define (get-bunny-storage-files directory)
  (map
    vm_extractor
    (with-input-from-request
      (make-request method: 'GET
                    uri: (uri-reference (textual->string (textual-append
                                          "https://storage.bunnycdn.com/"
                                          storage_zone_name "/"
                                          (textual-append directory "/"))))
                    headers: (headers (list (list 'AccessKey storage_access_key)
                                            '(accept "*/*"))))
      #f
      json-read)))

(define (put-bunny-storage-file filepath resource)
  (with-input-from-request
    (make-request method: 'PUT
                  uri: (uri-reference (textual->string (textual-append
                                                         "https://storage.bunnycdn.com/"
                                                         storage_zone_name "/"
                                                         "./"
                                                         filepath)))
                  headers: (headers (list (list 'AccessKey storage_access_key)
                                          (list 'ContentType "application/octet-stream")
                                          (list 'Checksum (alist-ref 'Checksum resource)))))
    (lambda ()
      (define body (call-with-input-file (alist-ref 'ObjectName resource)
                            (lambda (fh) (read-string #f fh))))
      (display body))
    read-string))

(define (delete-bunny-storage-file filepath)
  (with-input-from-request
    (make-request method: 'DELETE
                  uri: (uri-reference (textual->string (textual-append
                                                           "https://storage.bunnycdn.com/"
                                                           storage_zone_name "/"
                                                           "./"
                                                           filepath)))
                  headers: (headers (list (list 'AccessKey storage_access_key))))
    #f
    read-string))

(define (purge-bunny-pull-zone-cache)
  (with-input-from-request
    (make-request method: 'POST
                  uri: (uri-reference (textual->string (textual-append
                                                           "https://api.bunny.net/pullzone/"
                                                           pullzone_id "/"
                                                           "purgeCache")))
                  headers: (headers (list (list 'AccessKey pullzone_access_key))))
    #f
    read-string))

(define (sha256sum filepath)
  (textual->string (textual-upcase (message-digest-file (sha256-primitive) filepath))))

(define arguments (command-line-arguments))

(when (< (length arguments) 1)
  (display "Missing source directory for sync")
  (newline)
  (exit 1))

(define source_directory (car arguments))

(define file_map (make-hash-table))

(define (update_file_map path assoc_key data)
  (hash-table-update!/default file_map path (lambda (value) (alist-update assoc_key data value)) '()))

(map
  (lambda (filepath)
    (if (directory-exists? filepath) #f
      (update_file_map
        (textual->string (textual-drop filepath (add1 (textual-length source_directory))))
        'local
        (alist-update 'Checksum (sha256sum filepath)
                      (alist-update 'ObjectName filepath '())))))
  (find-files source_directory))

(define (with-generated-path directory resource)
  (alist-update 'Path
                (if (equal? directory ".") (alist-ref 'ObjectName resource)
                  (textual->string (textual-append directory "/" (alist-ref 'ObjectName resource))))
                resource))

(define-iterator (bunny-storage-files directory)
  (define files (get-bunny-storage-files directory))
  (do-list file files
           (if (not (alist-ref 'IsDirectory file))
             (yield (with-generated-path directory file))
             (yield-all
                (bunny-storage-files
                  (if (equal? "." directory)
                    (alist-ref 'ObjectName file)
                    (textual-append directory "/" (alist-ref 'ObjectName file)))))
             ))
  '())

(iterate (bunny-storage-files ".")
         (update_file_map (alist-ref 'Path it) 'remote it))


(define to_purge (make-hash-table))

(hash-table-map file_map
  (lambda (key value)
    (define local (alist-ref 'local value))
    (define remote (alist-ref 'remote value))
    (cond
      [(eq? #f local)
       (display (string-append "REMOVING " key)) (newline)
       (delete-bunny-storage-file key)
       (hash-table-set! to_purge key '())]
      [(eq? #f remote)
       (display (string-append "ADDING   " key)) (newline)
       (put-bunny-storage-file key local)]
      [(not (equal? (alist-ref 'Checksum remote) (alist-ref 'Checksum local)))
       (display (string-append "UPDATING " key)) (newline)
       (put-bunny-storage-file key local)]
      [else (display (string-append "SKIPPING " key)) (newline)])))

(when (> (hash-table-size to_purge) 0)
  (display "Purgin pull zone cache")
  (newline)
  (purge-bunny-pull-zone-cache))
