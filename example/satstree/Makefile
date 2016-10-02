all: test_prog

test_prog: main.dats out/c2ats_prelude.sats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC

out/c2ats_prelude.sats: example.h
	c2ats gen -o out $<

test: all
	./test_prog

clean:
	rm -rf example.sats *_dats.c *.o test_prog out

.PHONY: all clean test
