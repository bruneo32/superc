# SuperC: C11 superset language

### Index
- [Current developed features](#current-developed-features)
  - [Defer](#defer)
  - [Type methods](#type-methods)
- [Current planned features](#current-planned-features)
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

# Current planned features
> Please note that the **syntax** of all the planned features is **subject to change**.

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
