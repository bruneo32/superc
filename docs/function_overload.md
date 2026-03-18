---
title: Function overload
layout: blog
---

# Function overload
**SuperC** allows function overload with manual symbol mangling.

When a **function call** is made in **SuperC**, the compiler will not search the function only by name, it will search the function by name and signature, which means that when you have two functions with the same name but different type of parameters, the compiler will call the one that matches *(in name, and parameter types; but not return type)*.

This means that the collision of the **symbols** only happen in the linker, so if you have two functions with the same symbol, the linker will throw an **error**; but the compiler won't throw an error if they have the same name, but different **symbols** *(and signature of course)*.

> This is what [_Generic](<https://en.wikipedia.org/wiki/C11_(C_standard_revision)#Changes_from_C99>){:target="_blank"} does in C11, but made easy.

> Note: Variadic functions cannot be overloaded

### Examples
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
```c
#include <stdio.h>

/* Methods don't collide by default,
 * so no attribute symbol is needed */

int (int a) sum(int b) {
  // __func__ will print the symbol
  // of the function
  printf("1: %s\n", __func__);
  return a + b;
}

int (int a) sum(double b) {
  // __func__ will print the symbol
  // of the function
  printf("2: %s\n", __func__);
  return a + (int)b;
}

int main(void) {
  int x = 10;
  printf("=> %d, %d\n", x.sum(x), x.sum(11.1));

  // 2: sum$id
  // 1: sum$ii
  // => 20, 21

  return 0;
}
```
```c
#include <stdio.h>

int add(int a, int b) {
  return a + b;
}

int sum(int a, int b) {
  return add(a, b);
}

int sum(short a, short b) {
  return a - b;
}

int main(void) {

  // Function 'sum' has two overloads, so
  // specify the parameters in the reference
  // to select the overload desired
  int (*foo)(int, int) = sum(int, int);
  int (*bar)(short, short) = sum(short, short);

  printf("foo: %d\n", foo(7, 3));
  printf("bar: %d\n", bar(7, 3));

  // Function 'add' has no overload, so
  // there is no need to specify parameters in the reference
  int (*add2)(int, int) = add;
  printf("add2: %d\n", add2(7, 3));

  // foo: 10
  // bar: 4
  // add2: 10

  return 0;
}
```
