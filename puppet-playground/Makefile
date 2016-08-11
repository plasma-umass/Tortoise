run:
	docker run --rm="true" -v $(shell pwd)/manifests:/root/manifests:rw -i -t -h puppet-testing plasma/puppet-testing

build:
	docker build -t plasma/puppet-testing .

build-no-cache:
	docker build -no-cache -t plasma/puppet-testing .