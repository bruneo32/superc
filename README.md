# SuperC: C11 superset language

### Index
- [Current developed features](#current-developed-features)
  - [Defer](#defer)
  - [Defer in loops](#defer-in-loops)
  - [Type methods](#type-methods)
  - [break N](#break-n)
  - [Symbol mangling](#symbol-mangling)
- [Current planned features](#current-planned-features)
  - [Aliases](#aliases)
  - [Lambdas](#lambdas)

# Current developed features
## Defer
Will execute the code stated right before exiting a function
### Examples
```c
#include <stdio.h>

int main() {
  defer printf("This will print after OK\n");
  printf("OK\n");
  // OK
  // This will print after OK
  return 0;
}
```
```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  srand(time(NULL));

  FILE *f = fopen("test.txt", "w");
  defer {
    fclose(f);
    printf("File closed!\n");
  }

  /* 50% change to exit early with error */
  if ((rand() & 1) == 0) {
    printf("No need to call fclose(), because the defer executes before the return.\n");
    return 1;
  }

  fprintf(f, "Hello world!\n");

  return 0;
}
```
```c
#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

static SDL_Window   *game_window;
static SDL_Renderer *game_renderer;

int main() {
  /* Init SDL */
  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    return 1;
  defer SDL_Quit();

  /* Create window */
  game_window = SDL_CreateWindow("Defer test",
                    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                    640, 480, SDL_WINDOW_SHOWN);
  if (!game_window)
    return 2;
  defer SDL_DestroyWindow(game_window);

  /* Create renderer */
  game_renderer = SDL_CreateRenderer(game_window, -1,
                    SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
  if (!game_renderer)
    return 3;
  defer SDL_DestroyRenderer(game_renderer);

  /*
   * This example shows a basic usage of defer.
   * If function returns early, it will execute the defer statements before
   * exiting the function.
   * I.E.:
   *   - If game_renderer is NULL, SDL_DestroyWindow and SDL_Quit will be called
   *     (in that order, reverse to declaration), but SDL_DestroyRenderer won't.
   *     Because the return happens before the defer is declared.
   *
   * [!!!]
   * Beware that defer statements are related to the function, not the block of code.
   * This means that even though the following if statement never runs,
   * the following defer is declared and will beexecuted because `main`
   * does not return before the defer is declared.
   */
  if (1 == 0) {
    defer printf("Actual defer execution\n");
    return 4;
  }

  printf("OK\n");
  return 0;
}
```

## Defer in loops
Will execute the code stated right before the break/continue label of a for/while/do loop.
At first glance it can look totally unnecessary, but for memory management escenarios can be very useful.

### Examples
```c
#include <stdio.h>

int main() {
  for (int i = 0; i < 3; i++) {
    defer printf("This will run when the functions is over. Nothing to do with the loop\n");
    defer break printf("[%d] This will run right before loop break\n", i);
    defer continue printf("[%d] This will run right before loop increment\n", i);
  }

  printf("OK\n");
  // [0] This will run right before loop increment
  // [1] This will run right before loop increment
  // [2] This will run right before loop increment
  // [3] This will run right before loop break
  // OK
  // This will run when the functions is over. Nothing to do with the loop
  return 0;
}
```

## Type methods
Extends a type with implicit function calls.
It is equivalent of passing the caller as the first argument.
- Function is registered as `(type).name` *(the symbol is mangled, see [symbols](#symbol-mangling))*.
- **I.E.** `mycat.meow()` is exactly the same to `(Cat).meow(mycat)`.

### Examples
```c
#include <stdio.h>

int (int a) sum(int b) {
  return a + b;
}

int main() {
  // = 15
  printf("= %d\n", ((int)10).sum(5));
  // = 15
  printf("= %d\n", (int).sum(10, 5));
  return 0;
}
```
```c
#include <stdio.h>

struct color {
  unsigned char r, g, b, a;
};

typedef struct Car Car;
struct Car {
  char *name;
  struct color col;
  int kms;
};

void (Car *c) drive(int kms) {
  c->kms += kms;
}

int main() {
  Car c = {
    .name = "Kachow",
    .col = {
      .r = 255,
      .g = 0,
      .b = 0,
      .a = 255,
    },
    .kms = 3,
  };

  (&c).drive(100);

  Car *p = &c;
  p.drive(100);

  // Kachow has driven 203 kms
  printf("%s has driven %d kms\n", c.name, c.kms);
  return 0;
}
```

## break N
Will break out of a loop or switch N levels up.

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

## Symbol mangling
Allows the user to change the output symbol of a variable or function.
- New attribute `__attribute__((symbol("new_symbol_name")))` -> changes the output symbol at assembly level of the variable or function.
- New keyword `symbolof(identifier)` -> returns the expected symbol name of the variable or function as a string literal.

### Examples
```c
#include <stdio.h>

int foo __attribute__((symbol("bar"))) = 123;
int bar2 = 456; // No symbol change.

int sum(int a, int b) __attribute__((symbol("abc")));
int (int a) sum(int b) __attribute__((symbol("def"))); // was "sum$i"

// void abc(); // Link error: redefinition of symbol 'abc'

asm(".global __asm_var__\n"
    ".data\n.type __asm_var__, @object\n"
    ".size __asm_var__, 8\n.align 8\n"
    "__asm_var__: .long 32\n");
extern long ext1 __attribute__((symbol("__asm_var__")));

void test_symbol(char *a, char *b) {
  printf("%s == %s -> %d\n", a, b, strcmp(a, b) == 0);
}

int main(void) {
  test_symbol(symbolof(foo),       "bar");
  test_symbol(symbolof(bar2),      "bar2");
  test_symbol(symbolof(sum),       "abc");
  test_symbol(symbolof((int).sum), "def");

  test_symbol(symbolof(ext1), "__asm_var__");
  printf("ext1 = %d\n", ext1);

  // bar == bar -> 1
  // bar2 == bar2 -> 1
  // abc == abc -> 1
  // def == def -> 1
  // __asm_var__ == __asm_var__ -> 1
  // ext1 = 32
  // OK
  printf("OK\n");
  return 0;
}
```

# Current planned features
> Please note that the **syntax** of all the planned features is **subject to change**.

## Aliases
Register a declaration as an alias of another symbol. Very useful for [type methods](#type-methods).

### Examples
```c
#include <stdio.h>

void foo() {
  printf("foo\n");
}

void bar() __attribute__((alias(foo)));

int main() {
  foo();
  bar();
  printf("%p == %p", foo, bar);
  // foo
  // foo
  // 0x7fff5fb3b3e0 == 0x7fff5fb3b3e0
  return 0;
}
```
```c
#include <stdio.h>

typedef struct Point Point;
struct Point {
  double x, y;
};

Point point_add(Point p1, Point p2) {
  return (Point){
    .x = p1.x + p2.x,
    .y = p1.y + p2.y,
  };
}

/* No new function is emitted, just the compiler replaces the call with the aliased function */
Point (Point p1) add(Point p2) __attribute__((alias(point_add)));

int main() {
  Point p = { .x = 1, .y = 2 };
  Point q = { .x = 7, .y = -1 };

  Point r = p.add(q);

  // 8.0, 1.0
  printf("%f, %f\n", r.x, r.y);
  return 0;
}
```

## Lambdas
Lambdas are anonymous functions that can be assigned to variables, or used immediately.

### Examples
```c
#include <stdio.h>

int main() {
  auto add = int (int a, int b) {
    return a + b;
  }

  // 5
  printf("%d\n", add(2, 3));
  return 0;
}
```
```c
#include <stdio.h>

void sort(int *a, int n, int (*cmp)(int, int)) {
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (cmp(a[i], a[j]) > 0) {
        int tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
      }
    }
  }
}

int main() {
  /* Unordered array */
  int a[10] = { 4, 1, -5, 1, 3, 2, 6, 8, 9, 7 };

  sort(a, 10, int (int a, int b) {
    /* Return 1 if a > b */
    return a - b;
  });

  /* Print out the ordered array */
  for (int i = 0; i < 10; i++)
    printf("%d ", a[i]);
  putchat('\n');

  return 0;
}
```

## Notes
> Forked from [chibicc](https://github.com/rui314/chibicc).

Tip for debugging with gdb.
File `~/.gdbinit`:
```
set disable-randomization on
set follow-fork-mode child
catch syscall exit
catch syscall exit_group
catch signal SIGSEGV
catch signal SIGABRT
break error
```
