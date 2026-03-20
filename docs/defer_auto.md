---
title: (DRAFT) Defer auto
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Defer auto is currently **not implemented**. Syntax may change

# Defer in scoped blocks
> Will execute the code stated right before exiting a syntactical scope.

Sometimes ti's useful to execute code after a scoped block, not only in a function scope.

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

# Possible cancellation points
> This is still up to debate, we cover ***goto***, we have not talked about ***[longjmp](<https://gist.github.com/MangaD/b10d6a4c50e80712bc582e5968ae8bd0>)***, ***_Noreturn*** functions or signals.

The current idea *(and this can be changed)* is that the user *may* or *may not* want to run defers at some cancellation points, because maybe the user wants to fast-fail among other things.

Also, some cancellation points are easy to catch, but some are impossible, like [signals](<https://www.geeksforgeeks.org/c/signals-c-language/>){:target="_blank"}.

Nowadays, the design leverage is to use a `__builtin_unwind_defers` function that unwinds all the defers and the user can call it.

```c
if (fatal) {
  if (should_cleanup) {
    __builtin_unwind_defers();
  }
  panic("fatal error");
}
```

```c
#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>

jmp_buf env;

void deep_function() {
  int *str = malloc(sizeof(int));
  defer free(p);

  FILE *f = fopen("data.txt", "r");
  defer fclose(f);

  ...

  {
    ...
    // throw error
    // Without __builtin_unwind_defers(), defers are lost
    longjmp(env, 1);
    ...
  }

  ...

}

int main() {
  if (setjmp(env) == 0) {
    // try
    deep_function();
    printf("No error\n");
  } else {
    // catch
    printf("Error via longjmp!\n");
    exit(1);
  }
  return 0;
}
```
