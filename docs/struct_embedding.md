---
title: (DRAFT) Struct embedding
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Struct traits are currently **not implemented**. Syntax may change<br>

# Struct embedding
Struct embedding refers to the ability of a struct to:
- copy members from another struct ***(composition)***
- inherit behaviour from another struct ***(inheritance)***

# Struct composition
- A struct can copy members from another *(or more)* struct as an ***anonymous member***.
- You can specify the *[offset](<https://www.geeksforgeeks.org/c/structure-member-alignment-padding-and-data-packing/>){:target="_blank"}* where the *anonymous member* will be placed.
- All the *anonymous members* of a struct are considered ***parents*** of the struct. See [struct inheritance](#struct-inheritance).

{% tabs struct_inheritance1 %}
{% tab struct_inheritance1 SuperC %}
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
  struct A; // copy members from struct A (at this exact offset)
  int f;
  struct B; // copy members from struct B (at this exact offset)
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
  struct C c = {
    .b = 1, // from struct A
    .c = 2, // from struct B
  };

  // struct C has 7 members of type int (4 bytes)
  // so sizeof(c) = 7 * 4 = 28
  printf("struct C: %ld bytes\n", sizeof(c));
  // struct C: 28 bytes
  return 0;
}
```
{% endtab %}

{% tab struct_inheritance1 C99 %}
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
  struct A struct_a;
  int f;
  struct B struct_b;
  int g;
};

int main(void) {
  struct C c = {
    .struct_a.b = 1,
    .struct_b.d = 2,
  };

  printf("struct C: %ld bytes\n", sizeof(c));
  // struct C: 28 bytes
  return 0;
}
```
{% endtab %}
{% endtabs %}

## Extract anonymous members
Even if a member is *anonymous*, you can still extract it or point to it, just by casting it to its ***parent*** type.

- Casting a struct pointer to another *(that is **not a parent**)* is just a reinterpretation of that pointer type (same as classic **C**).
  - **i.e.**, `(struct D*)&c` takes `&c + 0` as a `struct D*` instead of a `struct C*`.
- Casting a struct to another *that is a **parent***, not only makes a cast, but also performs an offset to the base of the ***parent***.
  - **i.e.**, `(struct B*)&c` takes `&c + 12` because `struct B` starts at offset `12` inside `struct C`.
  - *Technically*, this is not a *zero-cost abstration*, because it performs an *offset*; which is just **one assembly instruction**, I hope that you can spare me.
  - If you put the struct at offset `0`, then `(struct B*)&c` will take `&c + 0`, making your program **1** cycle **faster**!

| Cast                         | Behavior         |
| ---------------------------- | ---------------- |
| `(struct D*)&c` (not parent) | reinterpret cast |
| `(struct B*)&c` (parent)     | pointer + offset |
| `(struct B*)(void*)&c`       | reinterpret cast. See [opaque casting](#opaque-casting-to-parent) |

> **Note:** This does not break retrocompatibility with **C23/C11**, since a struct cannot have ***parents*** in **C**.<br>

{% tabs struct_inheritance2 %}
{% tab struct_inheritance2 SuperC %}
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
  int __padding0;
  struct A;
  struct B;
};

int sum_of_A(struct A a) {
  return a.a + a.b;
}

int sum_of_B_ptr(struct B *b) {
  return b->a + b->b;
}

int main() {
  struct C c = {
    .a = 1, .b = 2, // from struct A
    .c = 3, .d = 4, // from struct B
  };

  // copy value of struct A inside struct C
  struct A a_from_c = (struct A)c; // applies offset
  printf("sum of A: %d\n", sum_of_A(a_from_c));

  // get a pointer to struct B inside struct C
  struct B *b_from_c = (struct B*)&c; // applies offset
  printf("sum of B: %d\n", sum_of_B_ptr(b_from_c));

  // sum of A: 3
  // sum of B: 7
  return 0;
}
```
{% endtab %}

{% tab struct_inheritance2 C99 %}
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
  int __padding0;
  struct A struct_a;
  struct B struct_b;
};

int sum_of_A(struct A a) {
  return a.a + a.b;
}

int sum_of_B_ptr(struct B *b) {
  return b->a + b->b;
}

int main() {
  struct C c = {
    .struct_a = {
      .a = 1,
      .b = 2,
    },
    .struct_b = {
      .c = 3,
      .d = 4,
    },
  };

  // copy value of struct A inside struct C
  struct A a_from_c = c.struct_a;
  printf("sum of A: %d\n", sum_of_A(a_from_c));

  // get a pointer to struct B inside struct C
  struct B *b_from_c = &c.struct_b;
  printf("sum of B: %d\n", sum_of_B_ptr(b_from_c));

  // sum of A: 3
  // sum of B: 7
  return 0;
}
```
{% endtab %}
{% endtabs %}

## Opaque casting to parent
What if you want to cast a struct to a ***parent*** type without performing any offset?
- You can reinterpret a struct pointer to a ***parent*** without offset, by casting to `void*` before.

```c
// This will not offset the pointer, so it's a regular cast
Animal *a = (Animal*)(void*)&mycat;
```

# Struct inheritance
> When a struct has composition, it also has inheritance.

All the anonymous members of a struct are considered ***parents***, and the struct will inherit any [type method](type_methods.md) from them.

{% tabs struct_inheritance3 %}
{% tab struct_inheritance3 SuperC %}
```c
#include <stdio.h>


// class Animal
typedef struct Animal Animal;
struct Animal {
  int age;
  float weight;
};

void (Animal *a) grow() {
  a->age++;
  a->weight += 1.25;
}


// class Dog
typedef struct Dog Dog;
struct Dog {
  char *name;
  Animal; // copy members from Animal
  int force;
};
// ===> Dog inherits grow from Animal


// class Cat
typedef struct Cat Cat;
struct Cat {
  char *name;
  Animal;
  int speed;
};

// override Animal.grow for Cat
void (Cat *c) grow() {
  c->age++;
  c->speed++;
}


int main() {
  Dog mydog = {
    .name = "Rover",
    .age = 2,
    .weight = 5.0,
    .force = 10,
  };

  (&mydog).grow(); // will call Animal.grow(&mydog + (char*))
  printf("My dog %s is %d years old, weights %.2f kg\n", mydog.name, mydog.age, mydog.weight);
  // My dog Rover is 3 years old, weights 6.25 kg

  Cat mycat = {
    .name = "Mr. Pants",
    .age = 3,
    .weight = 5.0,
    .speed = 10,
  };

  (&mycat).grow(); // Cat's don't get extra weight
  printf("My cat %s is %d years old, weights %.2f kg\n", mycat.name, mycat.age, mycat.weight);
  // My cat Mr. Pants is 4 years old, weights 5.0 kg

  return 0;
}
```
{% endtab %}

{% tab struct_inheritance3 C99 %}
```c
#include <stdio.h>


// class Animal
typedef struct Animal Animal;
struct Animal {
  int age;
  float weight;
};

void Animal_grow(Animal *a) {
  a->age++;
  a->weight += 1.25;
}


// class Dog
typedef struct Dog Dog;
struct Dog {
  char *name;
  Animal parent_animal;
  int force;
};


// class Cat
typedef struct Cat Cat;
struct Cat {
  char *name;
  Animal parent_animal;
  int speed;
};

// override Animal.grow for Cat
void Cat_grow(Cat *c) {
  c->parent_animal.age++;
  c->speed++;
}


int main() {
  Dog mydog = {
    .name = "Rover",
    .parent_animal = {
      .age = 2,
      .weight = 5.0,
    },
    .force = 10,
  };

  Animal_grow(&(mydog.parent_animal));
  printf("My dog %s is %d years old, weights %.2f kg\n", mydog.name, mydog.parent_animal.age, mydog.parent_animal.weight);
  // My dog Rover is 3 years old, weights 6.25 kg

  Cat mycat = {
    .name = "Mr. Pants",
    .parent_animal = {
      .age = 3,
      .weight = 5.0,
    },
    .speed = 10,
  };

  Cat_grow(&mycat); // Cat's don't get extra weight
  printf("My cat %s is %d years old, weights %.2f kg\n", mycat.name, mycat.parent_animal.age, mycat.parent_animal.weight);
  // My cat Mr. Pants is 4 years old, weights 5.0 kg

  return 0;
}
```
{% endtab %}
{% endtabs %}

## Struct encapsulation
Along with *inheritance* and *composition*, **SuperC** also support a set of read and write access permissions for struct ***members*** and ***methods***. See the ***[access](attributes.md#__attribute__access)*** attribute.

- All *members* and *methods* are ***public*** by default
- All the *members* can be *defined* in the struct **literal initializator**. *If this is a concern for you, see [constructors with encapsulation](soft_objects.md#constructors-with-encapsulation)*

> **Do not forget this!** Encapsulation in **SuperC** is only a *compile-time* **harness** to prevent naive **errors** in complex codebases. But **malicious** access to **private** and **protected** data is always **possible** in low level programming, as you can see the "HACKED" examples below. It's not a bug, it's how computers work.

{% tabs struct_inheritance4 %}
{% tab struct_inheritance4 simple %}
```c
#include <stdio.h>
#include <stdbool.h>

#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

struct Account {
  bool   is_active      private;
  double accountBalance private;
} [[packed]];

void (struct Account *a) transfer(double value) {
  // Perform here checks like:
  // - is value valid?
  // - is this account active?
  // - ...
  a->accountBalance += value;
}

double (struct Account a) balance() {
  return a.accountBalance;
}

int main() {
  struct Account acc = {
    // Literal initialization is permitted
    .accountBalance = 123.0
  };

  // Compile error: cannot modify accountBalance
  // acc.accountBalance = 999999.99;

  (&acc).transfer(100.5);
  printf("Account balance: %.2f\n", acc.balance());

  // Account balance: 223.50
  return 0;
}
```
{% endtab %}
{% tab struct_inheritance4 members %}
```c
#include <stdio.h>

#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

struct A {
  int pub public; // Already was public by default
  int prv private;
  int prt protected;
};

void (struct A a) dostuff() {
  // Type method with receiver "struct A"
  // has full access to protected and private
  a.pub = 10;
  a.prv = 11;
  a.prt = 12;
}

struct B {
  struct A; // inherit from struct A
  int prv_b private;
}

void (struct B b) dostuff() {
  // Receiver "struct B" inherits from 'struct A'
  // so this scope has full access to
  // protected members from 'struct A', but not privates
  b.pub = 13;
  b.prt = 14;
  b.prv_b = 15;
  // b.prv = 16;  // Error: 'prv' is private for 'struct A'
}

int main() {
  struct A stt_a = {1,2,3};
  stt_a.pub = 7;     // public is accessible anywhere
  // stt_a.prv = 8;  // Error: 'prv' is private for 'struct A'
  // stt_a.prt = 9;  // Error: 'prt' is protected for 'struct A'
  return 0;
}
```
{% endtab %}
{% tab struct_inheritance4 readonly access %}
```c
#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

// In this kind of struct, you really
// don't want anyone messing with
// 'capacity' or 'size', but still
// 'size' can be publicly readable

typedef struct IntList IntList;
struct IntList {
  int *data;
                  // read is public, write is private
  size_t size     [[access(public,private)]];
  size_t capacity private;
};

// ... here goes methods to manipulate size and capacity safely ...
```
{% endtab %}
{% tab struct_inheritance4 methods %}
```c
#include <stdio.h>

#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

struct A {
  int abc;
};

// Already was public by default
void (struct A a) pub_func() public {
  printf("A.pub_func\n");
}

void (struct A a) prv_func() private {
  printf("A.prv_func\n");
}

void (struct A a) prt_func() protected {
  printf("A.prt_func\n");
}

struct B {
  struct A; // inherit from struct A
};

void (struct B b) dostuff() {
  b.pub_func();
  b.prt_func();
  // b.prv_func();  // Error: 'prv_func()' is private for 'struct A'
}

int main() {
  struct A stt_a = {1};
  stt_a.pub_func();     // public is accessible anywhere
  // stt_a.prv_func();  // Error: 'prv_func()' is private for 'struct A'
  // stt_a.prt_func();  // Error: 'prt_func()' is protected for 'struct A'
  return 0;
}
```
{% endtab %}
{% tab struct_inheritance4 hacked1 %}
```c
// Imagine that Account type is very well protected.
// We can still easily hoist the private data
// Method 1. By creating a new type method

#include <stdio.h>

// From SIMPLE example
#include "account.h"

void (struct Account *a) myhack() {
  a->accountBalance += 999999.99;
}

int main() {
  struct Account acc = {0, 123.0};

  (&acc).myhack();

  printf("Account balance: %.2f\n", acc.balance());
  // Account balance: 1000122.99
  return 0;
}
```
{% endtab %}
{% tab struct_inheritance4 hacked2 %}
```c
// Imagine that Account type is very well protected,
// even using [[closed]].
// We can still easily access the private data
// Method 2. By knowing the offset of the data

#include <stdio.h>

// From SIMPLE example
#include "account.h"

int main() {
  struct Account acc = {0, 123.0};

  // Get a raw pointer to 'acc', plus the offset of bytes
  // to skip to access the data.
  // In this case, we have to skip 1 byte to get to 'accountBalance'
  double *accountBalance = 1 + (void*)&acc;
  *accountBalance = 696969.69;

  printf("Account balance: %.2f\n", acc.balance());
  // Account balance: 696969.69
  return 0;
}
```
{% endtab %}
{% endtabs %}
