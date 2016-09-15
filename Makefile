SUBDIRS := example

stack_build:
	stack build

stack_install:
	stack install

stack_build_prof:
	stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

stack_exec_prof:
	@echo "Run \"stack exec -- c2ats +RTS -p -RTS gen ...\"."

cabal_install:
	cabal install

all test:
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

clean:
	cabal clean
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: stack_build stack_install stack_build_prof stack_exec_prof cabal_install all test clean
