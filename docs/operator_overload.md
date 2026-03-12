---
title: Operator overload
layout: blog
---

# Operator overload
You can map an operator (+, +=, etc) to a type method call.

This can be very useful to avoid a headache when working with complex data types like strings and arrays.

But this is one of the features that could kill a **C** succesor, and has so many detractors because of the hidden code; so it has to be implemented wisely.

There are some rules that the compiler follows:
- You cannot overload primitive types.
  > `1+1` is 2, and **not a function call**
- You cannot pass a number to a pointer overload.
  This is meant to preserve pointer arithmetic.
  > `(char*)"Hello" + 2` is `"llo"`, and **not a function call**
- Assignment operators must return the receiver back and no other variable.
  > This alone eliminates 90% of surprises.
- Arithmetic operators parameters must be constant if they are pointers.
  > This prevents the user from modifying the value of the pointer

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
|   `~`    | `__del__(self)` |

### Examples
```c
#include <stdio.h>
#include <string.h>

/* char* += char* ===> s1.__iadd__(s2) */
char *(char *s1) __iadd__(const char *s2) __attribute__((symbol("strcat")));

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
```c
#include <stdio.h>

char *(char *s1) __iadd__(const char *s2) {
  char *s3 = strdup(s1);
  s3 = strcat(s3, s2);
  // return s3; // Error: the function shall always return the receiver 's1'
  s1 = s3;
  return s1; // Valid, you can modify s1 or not, but you must return s1
}

int main(void) {
  char str_hello[100] = "Hello";
  str_hello += " World";
  str_hello += "!";
  printf("%s\n", str_hello);
  return 0;
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
