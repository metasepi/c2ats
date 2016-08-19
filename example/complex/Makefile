all: test_prog

test_prog: main.dats example.sats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC

example.sats: example.c
	c2ats new $< > $@

test: all
	./test_prog

clean:
	rm -f example.sats *_dats.c *.o

.PHONY: all clean test
