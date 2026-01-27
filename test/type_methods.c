#include "test.h"

int sum(int a, int b) {
  return a - b;
}

int (int a) sum(int b) {
	return a + b;
}

struct cat {
	char* name;
	int age;
};

void (struct cat* c) roar() {
	printf("%s (%d): meow\n", c->name, c->age);
}

typedef struct Foo Foo;
struct Foo {
  int a;
  int (*sum)(Foo, int);
};

int (Foo f) sum2(int b) {
  return f.a + b;
}

typedef union Bar Bar;
union Bar {
  int (*sum)(Bar, int);
  int (*sub)(Bar, int);
};

int (Bar b) sum2(int a) {
  return 1 + a;
}

int main() {
  ASSERT(-1, sum(1, 2));
  ASSERT(3, ((int)1).sum(2));
  ASSERT(3, (int).sum(1, 2));

  struct cat mycat = { "Mr. Pants", 3 };
  (&mycat).roar();

  struct cat *mycat_ptr = &mycat;
  mycat_ptr.roar();

  Foo f = { 1, (Foo).sum2 };
  ASSERT(2, f.sum(f, 1));
  ASSERT(2, f.sum2(1));

  Bar b = { .sub = (Bar).sum2 };
  ASSERT(2, b.sum(b, 1));
  ASSERT(2, b.sum2(1));

  printf("OK\n");
  return 0;
}
