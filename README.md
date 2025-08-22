# SuperC: C11 superset language

## Features
- [Defer](#defer)
- [Type methods](#type-methods)

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
> I.E.: `int (int a) sum(int b)` ---> `int sum(int a, int b)`

### Examples
```c
#include <stdio.h>

int (int a) sum(int b) {
  return a + b;
}

int main() {
  // 15
  printf("%d\n", ((int)10).sum(5));
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

## Notes
> Forked from [chibicc](https://github.com/rui314/chibicc).
