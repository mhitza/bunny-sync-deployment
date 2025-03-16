bunny-sync-deployment: main.scm
	csc -static -o bunny-sync-deployment -L -lssl -L -lcrypto main.scm

zip: bunny-sync-deployment
	zip bunny-sync-deployment.zip bunny-sync-deployment

install: bunny-sync-deployment
	cp bunny-sync-deployment ~/.local/bin/bunny-sync-deployment

.PHONY: install
