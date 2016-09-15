all: test_prog

test_prog: main.dats example.sats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC `pkg-config --cflags --libs gtk+-3.0`

example.sats: example.h
	c2ats gen $< gcc `pkg-config --cflags gtk+-3.0` > $@

test: all
	./test_prog

clean:
	rm -f example.sats *_dats.c *.o test_prog

.PHONY: all clean test
