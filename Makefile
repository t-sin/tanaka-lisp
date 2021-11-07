.PHONY: all
all:
	make -C src

.PHONY: test
test:
	make -C src test
