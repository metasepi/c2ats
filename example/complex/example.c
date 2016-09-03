#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/dir.h>
/* example.c */
/* (tentative) definitions */
static c; extern c;
static int c = 3;
static int c;
int32_t d;
/* old style function definition */
void* match(s_ix, l_ix, pat)
  long l_ix;
  char unsigned *pat;
  { exit(1); }
#define P(A,B) (A+B)
/* typedefs and structs */
typedef struct s { struct { char chr; double dbl; } x,*y; } __attribute__((packed)) S;
struct t { int a; };
struct {
  unsigned int a : 10;
  unsigned int b : 8;
  unsigned int c : 12;
  unsigned int d : 2;
};
struct {
  void *(*traverse_func)(int a[10][20], void *f(int i));
  int a[2][3][4];
  int *b;
  struct t *c;
  int ***d;
};
typedef S T;
S f();
static g(S a)  { return 0; }
void *h(S f(S[3]));
struct t2 {
  int a;
  struct {
    int b;
    struct {
      int c;
    } d;
  } e;
};
enum e2 {
  E2_A,
  E2_B,
  E2_C
};
struct {
  void (*f)(enum e2, int[2][3], struct t2);
};
int main() { printf("%d\n",1); }
