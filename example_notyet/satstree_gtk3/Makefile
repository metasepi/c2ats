all: test_prog

test_prog: main.dats out/c2ats_prelude.sats config.hats
	patscc -o $@ $< -DATS_MEMALLOC_LIBC `pkg-config --cflags --libs gtk+-3.0`

out/c2ats_prelude.sats: example.h
	c2ats gen -c "gcc `pkg-config --cflags gtk+-3.0`" -o out $<

config.hats: out/c2ats_prelude.sats
	dirname `find out -name example.sats` | \
	  sed -e "s/^/#define EXAMPLE_targetloc \"/" | \
	  sed -e "s/$$/\"/" > $@

test: all
	@echo "run ./test_prog manually..."

clean:
	rm -rf example.sats *_dats.c *.o test_prog out config.hats

.PHONY: all clean test
