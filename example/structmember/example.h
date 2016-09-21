#include <stdio.h>

struct foo {
	int i;
	char *s;
};

struct bar {
	struct foo *f;
};

extern struct bar *alloc_bar(void);
extern void free_bar(struct bar *);
