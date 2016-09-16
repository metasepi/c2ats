SUBDIRS := example

build:
	stack build

install:
	stack install

build_prof:
	stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

run_prof:
	@echo "Run \"stack exec -- c2ats +RTS -p -RTS gen ...\"."

cabal_install:
	cabal install

all test:
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

clean:
	stack clean
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: build install build_prof run_prof cabal_install all test clean
