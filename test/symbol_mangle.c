#include "test.h"

int (void  a) func();
int (_Bool a) func();

int (char  a) func();
int (short a) func();
int (int   a) func();
int (long  a) func();

int (unsigned char  a) func();
int (unsigned short a) func();
int (unsigned int   a) func();
int (unsigned long  a) func();

int (float  a) func();
int (double a) func();
int (long double a) func();

int (int a[]) func();
int (int (*a)[10]) func();
int (int **a) func();

enum enum_named { A = 127, B };
int (enum enum_named a) func();

union union_named { int i; float f; };
int (union union_named a) func();

typedef void (*named_func_t)(enum enum_named, union union_named, int(*)[10], ...);
int (named_func_t a) func();

int main(void) {
  ASSERT(0, strcmp(symbolof((void).func),  "func$v"));
  ASSERT(0, strcmp(symbolof((_Bool).func), "func$b"));

  ASSERT(0, strcmp(symbolof((char).func),  "func$c"));
  ASSERT(0, strcmp(symbolof((short).func), "func$s"));
  ASSERT(0, strcmp(symbolof((int).func),   "func$i"));
  ASSERT(0, strcmp(symbolof((long).func),  "func$l"));

  ASSERT(0, strcmp(symbolof((unsigned char).func),  "func$h"));
  ASSERT(0, strcmp(symbolof((unsigned short).func), "func$t"));
  ASSERT(0, strcmp(symbolof((unsigned int).func),   "func$j"));
  ASSERT(0, strcmp(symbolof((unsigned long).func),  "func$m"));

  ASSERT(0, strcmp(symbolof((float).func),       "func$f"));
  ASSERT(0, strcmp(symbolof((double).func),      "func$d"));
  ASSERT(0, strcmp(symbolof((long double).func), "func$e"));

  ASSERT(0, strcmp(symbolof((int*).func),       "func$Pi"));
  ASSERT(0, strcmp(symbolof((int[]).func),      "func$Pi"));
  ASSERT(0, strcmp(symbolof((int[10]).func),    "func$Pi"));
  ASSERT(0, strcmp(symbolof((int**).func),      "func$PPi"));
  ASSERT(0, strcmp(symbolof((int*[10]).func),   "func$PPi"));
  ASSERT(0, strcmp(symbolof((int(*)[10]).func), "func$PA10i"));

  ASSERT(0, strcmp(symbolof((enum enum_named).func),   "func$E10enum_named"));
  ASSERT(0, strcmp(symbolof((union union_named).func), "func$U11union_named"));

  ASSERT(0, strcmp(symbolof((named_func_t).func),
            "func$PFE10enum_namedU11union_namedPA10iQv"));

  printf("OK\n");
  return 0;
}
