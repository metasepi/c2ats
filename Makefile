SUBDIRS := example

install:
	cabal install

all test:
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

clean:
	cabal clean
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: install all test clean
