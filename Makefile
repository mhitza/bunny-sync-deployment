bunny-sync-deployment: main.scm
	csc -static -o bunny-sync-deployment -L -lssl -L -lcrypto main.scm

install: bunny-sync-deployment
	cp bunny-sync-deployment ~/.local/bin/bunny-sync-deployment

.PHONY: install
