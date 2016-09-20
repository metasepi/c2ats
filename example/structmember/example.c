#include <stdlib.h>
#include "example.h"

struct foo *get_foo (void) {
	struct foo *ret;
	ret = malloc(sizeof(struct foo));
	if (NULL == ret) {
		return NULL;
	}
	ret->i = 10;
	ret->s = "FooBar";
	return ret;
}

void free_foo (struct foo *p) {
	if (NULL != p) {
		free(p);
	}
}
