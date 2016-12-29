all: test_prog

test_prog: main.dats example_welltyped.sats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC `pkg-config --cflags --libs gtk+-3.0`

gen:
	c2ats gen -c "gcc `pkg-config --cflags gtk+-3.0`" example.h > example_gen.sats

test: all
	@echo "run ./test_prog manually..."

clean:
	rm -f *_dats.c *.o test_prog

.PHONY: all gen clean test
