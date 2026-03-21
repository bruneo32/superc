---
title: (DRAFT) Defer auto
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Defer auto is currently **not implemented**. Syntax may change<br>
> Development is not going to advance until [LLVM backend](/#roadmap) is minimally stable.

# Defer in scoped blocks
> Will execute the code stated right before exiting a syntactical scope.

Sometimes it's useful to execute code after a scoped block, not only in a function scope.

> `defer auto` runs at...
> - the block scope end
> - the `goto` statements
> - the `continue`/`break` statements of a loop. See [defer in loops](#defer-in-loops)
> - the `return` statements, before the function-level defers.

### Examples
```c
#include <stdio.h>

int main() {
  int x = 8;

  {
    int *mem = malloc(sizeof(int));
    defer auto {
      free(mem);
      printf("mem is freed here\n");
    }

    *mem = 42 + x;
    printf("%d\n", *mem);
  }

  printf("OK\n");

  // 50
  // mem is freed here
  // OK
  return 0;
}
```
```c
#include <stdio.h>

int main() {
  int x = 8;

  {
    defer auto printf("I am unavoidable\n");
    // Try to escape defer execution
    goto lbl_end;
    printf("Unreachable\n");
  }

  lbl_end:
  printf("OK\n");

  // I am unavoidable
  // OK
  return 0;
}
```
```c
#include <stdio.h>
#include <stdbool.h>

int main() {
  while (true) {
    defer auto printf("A\n");
    while (true) {
      defer auto printf("B\n");
      while (true) {
        defer auto printf("C\n");
        // Exit 3 loops
        // Executing defers in LIFO order
        break 3;
      }
    }
  }

  printf("OK\n");
  // C
  // B
  // A
  // OK
  return 0;
}
```

# Defer in loops
In addition, `defer auto` has a special behavior in loops.

It runs also before the `continue`/`break` statements.
So the defer statement is executed at the end of every iteration.
This also applies for [break n](break_n.md).

### Examples
```c
#include <stdio.h>

int main() {

  for (int i = 0; i < 5; i++) {
    char *str = malloc(100); // 100 chars max
    defer auto free(str);

    if (i == 1)
      continue; // -> defer runs here

    sprintf(str, "Number: %d", i);
    printf("%s\n", str);

    if (i == 3)
      break; // -> defer runs here

    // -> defer runs here
  }

  printf("OK\n");

  // Number: 0
  // Number: 2
  // Number: 3
  // OK
  return 0;
}
```
