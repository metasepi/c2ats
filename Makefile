SUBDIRS := example

install:
	cabal install

run_prof:
	@echo "Run \"c2ats +RTS -p -RTS gen ...\"."

all test:
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

clean:
	cabal clean
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: build install build_prof run_prof all test clean
