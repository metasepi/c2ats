#include <stdlib.h>
#include "example.h"

struct bar *alloc_bar (void) {
	struct foo *foo;
	struct bar *bar;

	foo = malloc(sizeof(struct foo));
	if (NULL == foo) {
		return NULL;
	}
	foo->i = 10;
	foo->s = "FooBar";

	bar = malloc(sizeof(struct bar));
	if (NULL == bar) {
		free(foo);
		return NULL;
	}
	bar->f = foo;

	return bar;
}

void free_bar (struct bar *p) {
	free(p->f);
	free(p);
}
