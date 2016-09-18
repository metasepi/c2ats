#include "example.h"

struct foo g_foo = {1234, "Hello, world!"};
int g_int = 9191;
char *g_str = "Foo Bar";

#if 0
int main() {
	printf("g_foo = {%d, \"%s\"}\n", g_foo.i, g_foo.s);
	printf("g_int = %d\n", g_int);
	printf("g_str = \"%s\"\n", g_str);
	return 0;
}
#endif
