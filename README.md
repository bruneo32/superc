# SuperC: C11 superset language

### Index
- [Current developed features](#current-developed-features)
  - [Defer](#defer)
  - [Defer in loops](#defer-in-loops)
  - [Type methods](#type-methods)
  - [break N](#break-n)
  - [Symbol mangling](#symbol-mangling)
    - [Aliases](#aliases)
- [Current planned features](#current-planned-features)
  - [Operator overload](#operator-overload)
  - [Nullish coalescing operator](#nullish-coalescing-operator)
  - [Function override](#function-override)
  - [Struct inheritance](#struct-inheritance)
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

  /* 50% chance to exit early with error */
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
   * i.e.:
   *   - If game_renderer is NULL, SDL_DestroyWindow and SDL_Quit will be called
   *     (in that order, reverse to declaration), but SDL_DestroyRenderer won't.
   *     Because the return happens before the defer is declared.
   *
   * [!!!]
   * Beware that defer statements are related to the function, not the block of code.
   * This means that even though the following if statement never runs,
   * the following defer is declared and will be executed because `main`
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
At first glance it can look totally unnecessary, but for memory management scenarios can be very useful.

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
It is equivalent to passing the caller as the first argument.
- Function is registered as `(type).name` *(the symbol is mangled, see [symbols](#symbol-mangling))*.
- **i.e.** `mycat.meow()` is exactly the same to `(Cat).meow(mycat)`.

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
> Technically this can be done already in C with a **goto** statement, but this is syntactically more readable.

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

## Aliases
Symbol mangling can be used to create syntax aliases to existing variables and functions in the compiled binary.
Sure you can do `#define def abc`, but you cannot use preprocessor for [Type methods](#type-methods), so this is a useful feature of the symbol mangling.

```c
#include <stdio.h>
#include <string.h>

/* Silly GCC headers
 * destroy __attribute__ */
#ifdef __attribute__
#undef __attribute__
#endif

int abc = 32;
/* def is an alias of abc does not create a new variable */
extern int def __attribute__((symbol("abc")));

/* Use symbol mangling to create a function alias string.concat -> strcat */
char *(char *s1) concat(char *s2) __attribute__((symbol("strcat")));

int main(void) {
  char str_hello[100] = "Hello";
  str_hello.concat(" World").concat("!");
  printf("%d\n%s\n", def, str_hello);
  // 32
  // Hello World!
  return 0;
}
```
```c
#include <stdio.h>

/*
 * Question: How can I alias an inline function?
 * Answer: Inline the inliner
 */

inline int inline1(int n) { return n + 3; }
inline int (int i) inline2() { return inline1(i); }

/* Since both functions are inlined,
 * the binary result is the same as
 * calling just inline1(), so this
 * is an alias of inline1() */

int main(void) {
  /*
    binary result is exactly the same as
    printf("%d\n", 10 + 3);
    so this is an inline function alias
  */
  printf("%d\n", ((int)10).inline2());
  // 13
  return 0;
}
```

# Current planned features
> Please note that the **syntax** of all the planned features is **subject to change**.

## Operator overload
You can map an operator (+, +=, etc) to a type method call.

This can be very useful to avoid a headache when working with complex data types like strings and arrays.

But this is one of the features that could kill a **C** succesor, and has so many detractors because of the hidden code; so it has to be implemented wisely.

There are some rules that the compiler follows:
- You cannot overload primitive types.
  > `1+1` is 2, and **not a function call**
- You cannot pass a number to a pointer overload.
  This is meant to preserve pointer arithmetic.
  > `(char*)"Hello" + 2` is `"llo"`, and **not a function call**

### Binary arithmetic operators
| Operator |         Method         |
|:--------:|:-----------------------|
|   `+`    | `__add__(self, other)` |
|   `-`    | `__sub__(self, other)` |
|   `*`    | `__mul__(self, other)` |
|   `/`    | `__div__(self, other)` |
|   `%`    | `__mod__(self, other)` |

### Comparison operators
| Operator |         Method         |
|:--------:|:-----------------------|
|   `<`    | `__lt__(self, other)`  |
|   `>`    | `__gt__(self, other)`  |
|   `<=`   | `__le__(self, other)`  |
|   `>=`   | `__ge__(self, other)`  |
|   `==`   | `__eq__(self, other)`  |
|   `!=`   | `__ne__(self, other)`  |

### Assignment operators
| Operator |          Method         |
|:--------:|:------------------------|
|   `+=`   | `__iadd__(self, other)` |
|   `-=`   | `__isub__(self, other)` |
|   `*=`   | `__imul__(self, other)` |
|   `/=`   | `__idiv__(self, other)` |
|   `%=`   | `__imod__(self, other)` |

### Unary operators
| Operator |      Method     |
|:--------:|:----------------|
|   `-`    | `__neg__(self)` |
|   `+`    | `__pos__(self)` |
|   `~`    | `__inv__(self)` |

### Examples
```c
#include <stdio.h>
#include <string.h>

/* char* += char* ===> s1.__iadd__(s2) */
char *(char *s1) __iadd__(char *s2) __attribute__((symbol("strcat")));

int main(void) {
  char str_hello[100] = "Hello";
  str_hello += " World";
  str_hello += "!";
  printf("%s\n", str_hello);
  // Hello World!
  return 0;
}
```
```c
#include <stdio.h>
#include <string.h>

typedef struct Point Point;
struct Point {
  int x;
  int y;
};

/* Point + Point ===> p1.__add__(p2) */
Point (Point p1) __add__(Point p2) {
  /* structs are copied by value,
   * so we could have use p1 directly
   * without overwriting the original
   * p1 struct */
  Point p3;
  p3.x = p1.x + p2.x;
  p3.y = p1.y + p2.y;
  return p3;
}

bool (Point p1) __eq__(Point p2) {
  return p1.x == p2.x && p1.y == p2.y;
}

int main(void) {
  Point p1 = {1, 2};
  Point p2 = {3, 4};
  Point p3 = p1 + p2;
  printf("%d %d\n", p3.x, p3.y);
  // 4 6
  return 0;
}
```

## Nullish coalescing operator
The nullish coalescing operator is a ternary operator that returns its right operand if the left operand is null, and its left operand otherwise.

**i.e.:** `a ?? b` is equivalent to `(a != NULL) ? a : b`**

### Examples
```c
#include <stdio.h>
#include <string.h>

char *str_en_hello = "Hello";
char *str_es_hello = "Hola";

int main(void) {
  str_en_hello = NULL;
  /* Equivalent to: (str_en_hello != NULL) ? str_en_hello : str_es_hello */
  printf("%s\n", str_en_hello ?? str_es_hello);
  // Hola
  return 0;
}
```

## Function override
**SuperC** allows function override with manual symbol mangling.

When a **function call** is made in **SuperC**, the compiler will not check the function name, it will check the function signature, this means that when you have two functions with the same name but different type of parameters, the compiler will call the one that matches *(in name, return type and parameter types)*.

This means that the collision of the **symbols** only happen in the linker, so if you have two functions with the same symbol, the linker will throw an **error**; but the compiler won't throw an error if they have the same name, but different **symbols**.

> This is what [_Generic](https://en.wikipedia.org/wiki/C11_(C_standard_revision)#Changes_from_C99) does in C11, but made easy.

```c
#include <stdio.h>

void foo(float a) __attribute__((symbol("foo_f"))) {
  printf("FLOAT: %f\n", a);
}

void foo(int a)   __attribute__((symbol("foo_i"))) {
  printf("INT: %d\n", a);
}

// Examples of linker errors:
// void foo_f(); // Link error: redefinition of symbol 'foo_f'
// void bar() __attribute__((symbol("foo_f"))); // Link error: redefinition of symbol 'foo_f'

int main(void) {
  foo((int)   10);
  foo((float) 3.1415);
  // INT: 10
  // FLOAT: 3.141500
  return 0;
}
```

## Struct inheritance
A struct can inherit members from another (or more) struct, and also you can specify the order where the parent's members will be placed.

```c
#include <stdio.h>

struct A {
  int a;
  int b;
};

struct B {
  int c;
  int d;
};

struct C {
  int e;
  struct A; // Inherit members from struct A (at this offset)
  int f;
  struct B; // Inherit members from struct B (at this offset)
  int g;
};

// i.e. ===>
// struct C {
//   int e;
//   int a; // from struct A
//   int b; // from struct A
//   int f;
//   int c; // from struct B
//   int d; // from struct B
//   int g;
// };

int main(void) {
  struct C c;
  // struct C has 7 members of type int (4 bytes)
  // so sizeof(c) = 7 * 4 = 28
  printf("C: %ld B", sizeof(c));
  // C: 28 B
  return 0;
}
```
```c
#include <stdio.h>

struct color {
  unsigned char r, g, b, a;
};

typedef struct Vehicle Vehicle;
struct Vehicle {
  int doors;
};

typedef struct Car Car;
struct Car {
  Vehicle; // Inherit members from Vehicle
  int wheels;
  struct color;
};
/* Constructor */
#define Car(doors_, color_) ((Car){.wheels = 4, .doors = (doors_), .color = (color_)})

int main(void) {
  struct color c_red = {.r = 255, .g = 0, .b = 0, .a = 255};
  Car car = Car(4, c_red);
  printf("%d %d %d\n", c.a, c.b, c.c);
  // 1 2 3
  return 0;
}
```

## Lambdas
Lambdas are anonymous functions that can be assigned to variables, or used immediately.
> Capture is still on the thinking phase

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
  putchar('\n');

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
