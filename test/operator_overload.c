#include "test.h"

typedef struct myint myint;
struct myint {
  long _pad1;
  int x;
  long _pad2;
};

myint (myint a) __add__(int b) {
  a.x += b;
  return a;
}

myint (myint a) __sub__(int b) {
  a.x -= b;
  return a;
}

myint *(myint *a) __iadd__(myint b) {
  a->x += b.x;
  return a;
}

myint *(myint *a) __isub__(myint b) {
  a->x -= b.x;
  return a;
}

_Bool (myint a) __eq__(int b) {
  return a.x == b;
}

int main() {
  myint a = { .x = 1 };
  myint b = { .x = 5 };

  ASSERT(3, ({ myint c = a + 2; c.x; }) );
  ASSERT(3, (a + 2).x ); // Same as above
  ASSERT(1, a.x); // a unmodified

  ASSERT(&a, &a += b);
  ASSERT(6, a.x); // a modified

  a.x = 1;
  ASSERT(-1, ({ myint c = a - 2; c.x; }) );
  ASSERT(-1, (a - 2).x ); // Same as above
  ASSERT(1, a.x); // a unmodified

  ASSERT(&a, &a -= b);
  ASSERT(-4, a.x); // a modified

  ASSERT(0, a == 1);
  ASSERT(1, a == -4);

  printf("OK\n");
  return 0;
}
