all: test_prog

test_prog: main.dats example.c example.sats
	patscc -o $@ main.dats example.c -DATS_MEMALLOC_LIBC

example.sats: example.h
	c2ats gen $< > $@

test: all
	./test_prog

clean:
	rm -f example.sats *_dats.c *.o test_prog

.PHONY: all clean test
