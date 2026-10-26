---
title: Type methods
layout: blog
---

# Type methods
Under the hood, this is pure *syntactic sugar* for a function that takes the **"receiver"** as its first parameter, but the **ergonomic** and **architectural** benefits are anything but superficial.

- Function is registered as `(type).name` *(the symbol is mangled, see [symbols](symbols.md))*.
- **i.e.** `mycat.meow()` is exactly the same to `(Cat).meow(mycat)`.

> This is not only **easier**, but **safer**, because it prevents the programmer from passing a **wrong** type to the function like `player_move(&enemy)`.

{% tabs methods1 %}
{% tab methods1 SuperC %}
```cpp
#include <stdio.h>

typedef struct Cat Cat;
struct Cat {
  char *name;
};

void (Cat c) meow() {
  printf("Meow! My name is %s\n", c.name);
}

int main() {
  Cat mycat = {
    .name = "Mr. Pants",
  };

  mycat.meow();

  // Meow! My name is Mr. Pants
  return 0;
}
```
{% endtab %}

{% tab methods1 C99 %}
```c
#include <stdio.h>

typedef struct Cat Cat;
struct Cat {
  char *name;
};

void cat_meow(Cat c) {
  printf("Meow! My name is %s\n", c.name);
}

int main() {
  Cat mycat = {
    .name = "Mr. Pants",
  };

  cat_meow(mycat);

  // Meow! My name is Mr. Pants
  return 0;
}
```
{% endtab %}
{% endtabs %}

### Examples
{% tabs methods2 %}
{% tab methods2 primitives %}
```cpp
#include <stdio.h>

int (int a) sum(int b) {
  int c = a + b;
  printf("%d.sum(%d) = %d\n", a, b, c);
  return c;
}

int main() {
  // Two ways to call a method:
  ((int)10).sum(5);
  (int).sum(10, 6);

  // 10.sum(5) = 15
  // 10.sum(6) = 16
  return 0;
}
```
{% endtab %}

{% tab methods2 structs %}
```cpp
#include <stdio.h>

typedef struct Point Point;
struct Point {
  int x;
  int y;
};

void (Point p) move_as_copy(int x, int y) {
  // p is not the original, is a new struct
  // copied from the original
  p.x += x;
  p.y += y;
  // our new p dies here
}

void (Point *p) move(int x, int y) {
  // p is a pointer, referencing the
  // original struct
  p->x += x;
  p->y += y;
}

int main() {
  // Create a struct in the stack
  Point p = {.x = 0, .y = 0};

  // In C structs are passed by copy, so
  // the original is not modified!
  p.move_as_copy(10, 10);
  printf("p.x = %d, p.y = %d\n", p.x, p.y);

  // That is why struct methods usually
  // take a pointer, instead of the struct copy
  (&p).move(10, 10);
  printf("p.x = %d, p.y = %d\n", p.x, p.y);

  // Not all pointers have to live in the heap!
  // Create a struct on the stack, and
  // get a pointer to it
  Point *q = &(Point){.x = 3, .y = 4};
  q.move(10, 10);
  printf("q.x = %d, q.y = %d\n", q->x, q->y);

  // p.x = 0, p.y = 0
  // p.x = 10, p.y = 10
  // q.x = 13, q.y = 14
  return 0;
}
```
{% endtab %}

{% tab methods2 Function reference %}
```cpp
#include <stdio.h>

typedef struct Cat Cat;
struct Cat {
  char *name;
};

void (Cat c) meow() {
  printf("Meow! My name is %s\n", c.name);
}

int main() {
  Cat mycat = {
    .name = "Mr. Shirts",
  };

  // Get a function as variable named "fn_meow"
  void (*fn_meow)(Cat) = (Cat).meow;
  fn_meow(mycat);

  // Meow! My name is Mr. Shirts
  return 0;
}
```
{% endtab %}
{% endtabs %}
