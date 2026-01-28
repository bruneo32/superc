#include "test.h"

typedef struct myint myint;
struct myint {
  int x;
};

myint (myint a) __add__(int b) {
  return a.x + b;
}

myint *(myint *a) __iadd__(myint b) {
  a->x += b.x;
  return a;
}

int main() {
  myint a = { 1 };
  myint b = { 5 };

  ASSERT(3, ({ myint c = a + 2; c.x; }) );
  ASSERT(1, a.x); // a unmodified
  ASSERT(&a, &a += b);
  ASSERT(6, a.x); // a modified

  printf("OK\n");
  return 0;
}
