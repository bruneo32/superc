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

int main() {
  ASSERT(3, ((int)1).sum(2));
  ASSERT(3, (int).sum(1, 2));
  ASSERT(-1, sum(1, 2));

  struct cat mycat = { "Mr. Pants", 3 };
  (&mycat).roar();

  struct cat *mycat_ptr = &mycat;
  mycat_ptr.roar();

  printf("OK\n");
  return 0;
}
