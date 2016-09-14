SUBDIRS := $(wildcard */)

all test clean:
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: all test clean
