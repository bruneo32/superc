#include "test.h"

int sum(int a, int b) {
  return a - b;
}

int sum(short a, short b) __attribute__((symbol("sum2"))) {
  return a - b + 1;
}

// Automatic symbol mangling 'sum$ii'
int (int a) sum(int b) {
  return a + b;
}

// Automatic symbol mangling 'sum$ss'
// no collision with 'sum$ii'
int (short a) sum(short b) {
  return a + b - 1;
}
// Automatic symbol mangling 'sum$si'
int (short a) sum(int b) {
  return 100000;
}
// Automatic symbol mangling 'sum$sd'
int (short a) sum(double d) {
  return (int)d;
}

int main() {
  int x = 2;
  ASSERT(-1, sum((int)1, (int)2));
  ASSERT(3,  ((int)1).sum(x));
  ASSERT(3,  x.sum((int)1));
  ASSERT(4,  x.sum(x));
  ASSERT(0,  sum((short)1, (short)2));
  ASSERT(2,  ((short)1).sum((short)2));
  ASSERT(2,  ((short)1).sum((short)2));
  ASSERT(3,  ((short)1).sum(3.14));

  ASSERT(100000, ((short)1).sum((int)2));

  int (*foo)(int, int) = sum(int, int);
  int (*bar)(int, int) = (int).sum(int);
  ASSERT(-1, foo(1, 2));
  ASSERT(3, bar(1, 2));

  int (*foo2)(short, short) = sum(short, short);
  int (*bar2)(short, short) = (short).sum(short);
  ASSERT(0, foo2(1, 2));
  ASSERT(2, bar2(1, 2));

  printf("OK\n");
  return 0;
}
