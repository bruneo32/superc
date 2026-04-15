---
title: Soft objects (OOP)
layout: blog
---

**SuperC** is not an [Object-Oriented Programming](<https://en.wikipedia.org/wiki/Object-oriented_programming>){:target="_blank"} language *(OOP)*, but it supports some *OOP* features and patterns with **soft objects**.

- **Soft objects**: *zero-cost overhead* for most object-oriented scenarios, but losing some edge case *OOP* behaviour.
- **Full objects**: Still, you can write [full objects](#full-objects) in **SuperC**, which is close to real objects in *OOP*.

| Feature             | Soft OOP     | Full OOP                      |
| ------------------- | ------------ | ----------------------------- |
| **Dispatch**        | Compile-time | Runtime                       |
| **Polymorphism**    | Static only  | True polymorphism             |
| **Performance**     | Zero-cost    | Runtime overhead              |
| **Memory overhead** | None         | Extra *(vtable/function ptr)* |
| **Flexibility**     | Limited      | High                          |
| **Complexity**      | Low          | Higher                        |
| **Use case**        | Systems code | Flexible architectures        |

# Soft objects
A **soft object** is a sweet point between composition and inheritance, just a clever use of [type methods](methods.md), and [struct embedding](struct_embedding.md).

Does not comply with the official definition of *OOP*, but is close enough for most scenarios and introduces no runtime cost.

This can bring objects closer to low-level programming, speed up games runtime, etc.

## Constructors and destructors
```c
#include <stdio.h>

// class Animal
typedef struct Car Car;
struct Car {
  int doors;
  int wheels;
  float weight;
};

// constructor
inline Car *Car::new(int doors, int wheels) {
  Car *c = malloc(sizeof(Car));

  c->doors = doors;
  c->wheels = wheels;
  c->weight = 1000.0;

  return c;
}

// destructor: operator~
void (Car *c) __del__() {
  free(c);
}

int main() {
  Car *mycar = Car::new(4, 4);
  defer ~mycar; // destroy mycar at the end

  ...

  return 0;
}
```

## Call super methods
Although you can override a method from a ***parent***, you can also call the ***parent***'s method explicitly.
```c
// Cat inherits from Animal
void (Cat *c) grow() {
  Animal *super = (Animal*)c;
  super.grow();
  // or shortcut: ((Animal*)c).grow();
}
```

## Encapsulation
**SuperC** does not support encapsulation *yet* (private/protected members). Right now, there's a way to kindly **hint** the encapsulation of a member to a fellow programmer.

- A member can be marked as *private*/*protected* by prefixing the member name with `_` or `__`.
- As a programmer you are kindly asked to avoid raw access to private/protected members without knowing what you are doing.

```c
struct A {
  int abc;   // public
  int _def;  // protected
  int __fgh; // private
};

// getter for __fgh
int (struct A a) fgh() {
  return __fgh;
}
```

> Note: in future releases, encapsulation can be done by using some attribute like `__attribute__((private))`.

## Soft polymorphism - static dispatch
You can inherit [type methods](methods.md) from struct parent's members and override them.
- Since a **method** is binded to a **type**, polymorphism can be lost when casting to parent *(different type)*.
- But **maybe** you wanted this kind of polymorphism.

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

// class Cat
typedef struct Cat Cat;
struct Cat {
  char *name;
  Animal; // inherit from Animal
  int speed;
};

// override Animal.grow for Cat
void (Cat *c) grow() {
  c->age++;
  c->speed++;
}

int main() {
  Cat mycat = {
    .name = "Mr. Pants",
    .age = 3,
    .weight = 5.0,
    .speed = 10,
  };

  (&mycat).grow();          // Call Cat.grow
  ((Animal*)&mycat).grow(); // Call Animal.grow, real polymorphism is lost

  return 0;
}
```

# Full objects
There is no syntax for full *OOP* objects in **SuperC**, but you can write them like you could in classic **C**.

## Encapsulation
You can hide the private members of a class by not exposing them, and assigning a padding so the compiler knows the size of the struct, but does not know how to address the last secret members.

```c
// car.h - public header shipped along with the compiled library
struct Car {
// public:
  int color;
  int wheels;
// private:
  _Alignas(8) char __pad; // reserve 8 bytes for private members
};

// sizeof(Car) == 16
```

```c
// car.c - secret source code of the library distributed
struct Car {
// public:
  int color;
  int wheels;

// private:
// 2 * sizeof(int) = 8
  int __doors;
  int __speed;
};

// sizeof(Car) == 16
```

## Run-time type information (RTTI)
You can easily achieve [RTTI](<https://en.wikipedia.org/wiki/Run-time_type_information>){:target="_blank"}, with **macros** and a hidden number.

```c
// class.h
enum rtti_t : unsigned int {
  RTTI_NONE = 0,
  RTTI_CLASS = 1,
  RTTI_Vehicle,
  RTTI_Car,
  RTTI_Person,
  ...
};

#define IS_CLASS(obj_) ((obj_)->__rtti >= RTTI_CLASS)
struct Class {
  rtti_t __rtti;
}

// vehicle.h
#define IS_VEHICLE(obj_) ((obj_)->__rtti >= RTTI_Vehicle && (obj_)->__rtti <= RTTI_Car)
struct Vehicle {
  Class;
  int speed;
  int wheels;
}

// car.h
#define IS_CAR(obj_) ((obj_)->__rtti == RTTI_Car)
struct Car {
  Vehicle;
  int doors;
  int color;
}
```

## Full polymorphism - dynamic dispatch
Instead of soft polymorphism, you can write full polymorphism with **dynamic dispatch**.

```c
#include <stdio.h>

// class Animal
typedef struct Animal Animal;
struct Animal {
  int age;
  float weight;
  void (*_grow)(Animal *a);
};

inline void (Animal *a) grow() {
  a->_grow(a);
}

// class Cat
typedef struct Cat Cat;
struct Cat {
  char *name;
  Animal; // inherit from Animal
  int speed;
};

void Cat_grow(Cat *c) {
  c->age++;
  c->speed++;
}

// Cat constructor
inline Cat *Cat::new(char *name, int age, float weight, int speed) {
  Cat *c = malloc(sizeof(Cat));

  c->name = name;
  c->age = age;
  c->weight = weight;
  c->speed = speed;

  // override Animal.grow with dynamic dispatch
  c->_grow = Cat_grow;

  return c;
}

// destructor: operator~
void (Cat *c) __del__() {
  free(c);
}

int main() {
  Cat *mycat = Cat::new("Mr. Pants", 3, 5.0, 10);
  defer ~mycat;

  mycat.grow();             // Call Cat.grow
  ((Animal*)&mycat).grow(); // Call Cat.grow

  return 0;
}
```
