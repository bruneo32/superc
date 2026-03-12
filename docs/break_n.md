---
title: break N
layout: blog
---

# break N
Will break out of a loop or switch N levels up.
> Technically this can be done already in C with a `goto` statement, but this is syntactically more readable.

### Examples
```c
#include <stdio.h>

int main() {
  for (int j = 0; j < 10; j++) {
    for (int i = 0; i < 10; i++) {
      if (i == 5 && j == 2)
        break 2; /* Break both for loops */
      printf(" %d\n", i);
    }
    putchar('\n');
  }
  printf("\n -- End --\n");
  // 0 1 2 3 4 5 6 7 8 9
  // 0 1 2 3 4 5 6 7 8 9
  // 0 1 2 3 4
  // -- End --
  return 0;
}
```
