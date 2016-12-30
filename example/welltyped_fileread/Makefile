all: test_prog

test_prog: main.dats example_welltyped.sats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC

gen:
	c2ats gen example.h > example_gen.sats

test: all
	./test_prog

clean:
	rm -f *_dats.c *.o test_prog

.PHONY: all gen clean test
