**Experimental** commandline utility to sync a directory to a bunny.net storage zone and flush the related pull zone cache

```shell
cd docker-container
docker build -t chicken-build-environment .

# build binary
cd ..
docker run --rm -ti -v `pwd`:/code:z chicken-build-environment make
```
