#include <stdio.h>

struct foo {
	int i;
	char *s;
};

extern struct foo *get_foo(void);
extern void free_foo(struct foo *);
