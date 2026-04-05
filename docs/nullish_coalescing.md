---
title: Nullish coalescing operator
layout: blog
---

# Nullish coalescing operator
> This is already implemented in [GNU C](<https://gcc.gnu.org/onlinedocs/gcc/Conditionals.html>){:target="_blank"}

The nullish coalescing operator is a ternary operator that returns its **right** operand if the left operand is *NULL*, and its **left** operand otherwise.

**i.e.** `a ?: b` is equivalent to `a ? a : b`

> Note: that operator overload for `__ne__` does not work for nullish coalescing operator, because `(x != 0)` is not tested, it's just the variable "truthiness". If you want to test the operator overload, use explicit ternary operator `x != 0 ? x : y`.

### Examples
```c
#include <stdio.h>
#include <string.h>

void print_first_word(char *str) {
  // If str is 0 (NULL), s will be "unknown"
  char *s = str ?: "unknown";
  size_t len = strlen(s);

  for (int i = 0; i < len; i++) {
    if (s[i] == ' ')
      break;
    putc(s[i], stdout);
  }
  putc('\n', stdout);
}

int always_5() {
  int x = 0;
  // (x) ? x : 5
  return x ?: 5;
}

int main(void) {
  char *str_hello = "Hello world";
  print_first_word(str_hello);
  // Hello

  print_first_word(NULL);
  // unknown

  printf("x: %d\n", always_5());
  // x: 5

  return 0;
}
```
