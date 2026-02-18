---
title: Type methods
---

# Type methods
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
