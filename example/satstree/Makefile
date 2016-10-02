all: test_prog

test_prog: main.dats out/c2ats_prelude.sats config.hats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC

out/c2ats_prelude.sats: example.h
	c2ats gen -o out $<

config.hats: out/c2ats_prelude.sats
	dirname `find out -name example.sats` | \
	  sed -e "s/^/#define EXAMPLE_targetloc \"/" | \
	  sed -e "s/$$/\"/" > $@

test: all
	./test_prog

clean:
	rm -rf example.sats *_dats.c *.o test_prog out config.hats

.PHONY: all clean test
