all: test_prog

test_prog: main.dats example.sats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC -Wno-implicit-int

example.sats: example.h
	c2ats gen $< > $@

test: all
	./test_prog

clean:
	rm -f example.sats *_dats.c *.o test_prog

.PHONY: all clean test
