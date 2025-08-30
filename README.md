# SuperC: C11 superset language

### Index
- [Current developed features](#current-developed-features)
  - [Defer](#defer)
  - [Type methods](#type-methods)
  - [Symbol mangling](#symbol-mangling)
- [Current planned features](#current-planned-features)
  - [Aliases](#aliases)
  - [break N](#break-n)
  - [Lambdas](#lambdas)

# Current developed features
## Defer
Will execute the code stated right before exiting a block of code.
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

int main() {
  FILE *f = fopen("test.txt", "w");
  defer {
    fclose(f);
    printf("File closed!\n");
  }

  /* 50% change to exit early with error */
  if (rand() % 2 == 0) {
    printf("No need to call fclose(), because the defer executes before the return.\n");
    return 1;
  }

  fprintf(f, "Hello world!\n");

  return 0;
}
```

## Type methods
Extends a type with implicit function calls.
It is equivalent of passing the caller as the first argument.
- Function is registered as `(type).name` *(the symbol is mangled, see [symbols](#symbol-mangling))*. \
  **I.E.** `mycat.meow()` is exactly the same to `(Cat).meow(mycat)`.

### Examples
```c
#include <stdio.h>

int (int a) sum(int b) {
  return a + b;
}

int main() {
  // = 15
  printf("= %d\n", ((int)10).sum(5));
  // = 3
  printf("= %d\n", (int).sum(1, 2));
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

## Symbol mangling
Allows the user to change the output symbol of a variable or function.
- New attribute `__attribute((symbol("new_symbol_name")))` -> changes the output symbol at assembly level of the variable or function.
- New keyword `symbolof(identifier)` -> returns the expected symbol name of the variable or function as a string literal.

### Examples
```c
#include <stdio.h>

int foo __attribute__((symbol("bar"))) = 123;
int bar2 = 456; // No symbol change.

int sum(int a, int b) __attribute__((symbol("abc")));
int (int a) sum(int b) __attribute__((symbol("def"))); // was "sum$i"

// void abc(); // Error: redefinition of 'abc'

asm(".section .data\n"
    ".global __asm_var__\n"
    "__asm_var__: .long 32\n"
    ".section .text\n");
extern long ext1 __attribute__((symbol("__asm_var__")));

void test_symbol(char *a, char *b) {
  printf("%s == %s -> %d", a, b, strcmp(a, b) == 0);
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

## break N
Will break out of a scope N levels up.

### Examples
```c
#include <stdio.h>

int main() {
  for (int j = 0; j < 10; j++) {
    for (int i = 0; i < 10; i++) {
      if (i == 5 && j == 2)
        break 2; /* Break both for loops */
      printf(" %d", i);
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
