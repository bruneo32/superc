#include "test.h"

int foo __attribute__((symbol("bar"))) = 123;
int bar2 = 456;

int (int a) sum(int b) __attribute__((symbol("def")));
int (int a) add(int b) /* Symbol defaults to `add$i` */;
int sum(int a, int b) __attribute__((symbol("abc")));

asm(".section .data\n"
    ".global __asm_var__\n"
    "__asm_var__: .long 32\n"
    ".section .text\n");
extern long ext1 __attribute__((symbol("__asm_var__")));

int main(void) {
  ASSERT(0, strcmp(symbolof(foo), "bar"));
  ASSERT(0, strcmp(symbolof(bar2), "bar2"));
  ASSERT(0, strcmp(symbolof(sum), "abc"));
  ASSERT(0, strcmp(symbolof((int).sum), "def"));
  ASSERT(0, strcmp(symbolof((int).add), "add$i"));
  ASSERT(0, strcmp(symbolof(ext1), "__asm_var__"));
  ASSERT(32, ext1);

  printf("OK\n");
  return 0;
}
