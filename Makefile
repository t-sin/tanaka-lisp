.PHONY: build
build:
	make -C src build

.PHONY: test
test:
	make -C src test

.PHONY: clean
clean:
	make -C src clean
