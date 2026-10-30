---
title: (DRAFT) Soft objects (OOP)
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

typedef struct Car Car;
struct Car {
  int doors;
  int wheels;
  float weight;
};

// constructor
inline Car *Car::new(int doors, int wheels) {
  // Allocate
  Car *c = malloc(sizeof(Car));

  // Initialize
  c->doors = doors;
  c->wheels = wheels;
  c->weight = 1000.0;

  // Return
  return c;
}

// destructor: operator~
Car *(const Car *c) __del__() {
  free(c);
  // __del__ must return the receiver
  return c;
}

int main() {
  Car *mycar = Car::new(5, 4);
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

## Soft encapsulation
As discussed in [struct encapsulation](struct_embedding.md#struct-encapsulation), the **SuperC** compiler can prevent a user from *accessing* a struct's **member** or **method** at compile time.

> Again, note that this is just a **harness**, and does **not** mean or **guarantee**, that the **access** to that members is **impossible**.

```c
#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

typedef struct Animal Animal;
struct Animal {
  int age    private;
  int weight protected;
};

void (Animal *a) grow() protected {
  // Animal scope can access private 'age'
  a->age++;
}

typedef struct Cat Cat;
struct Cat {
  Animal; // inherit
};

void (Cat *a) grow() protected {
  // Cat cannot access 'age',
  // but can call super.grow
  ((Animal*)a).grow();
}
```

## Getters and setters
Remembering [function overload](function_overload.md), we can use the same name for two different **methods**.

**SuperC** will not adopt the syntax convention of `a.def = 3` calling `a.def(3)`. Because it flagrantly defies the principle of ***transparency*** for an OOP contract *(again, **SuperC** is not an OOP language)*.

> Note: It is **recommended** to mark a *private*/*protected* member also by **prefixing** the member **name** with `_` or `__`, so it does not **collide** with **getters** or **setters**.

```c
#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

struct A {
  int abc; // public by default
  int __def private;
};

// getter for __def
int (const struct A *a) def() {
  return a->__def;
}

// setter for __def
void (struct A *a) def(int value) {
  a->__def = value;
}

int main() {
  struct A *a = &(struct A){1,2};

  // Error: cannot access private member '__def'
  // a->__def = 3;

  a.def(99); // Use setter
  printf("a = (%d, %d)\n", a->abc, a.def()); // Use getter

  // a = (1, 99)
  return 0;
}
```

## Constructors with encapsulation
The previous [constructor](#constructors-and-destructors) example was simple *(and you should totally use it for simple things)*.

- **Problem:** the **user** can still access **private** fields in the struct's *literal initializer*
- **Solution:** use the *attribute* `noinit` to **enforce** that the struct **cannot** be *initialized* outside the declaration **file** *(not even in the same **translation unit**)*
- ---
- **Problem:** the **user** can hoist the private scope by creating a **custom method**
- **Solution:** use the *attribute* `closed` to **enforce** that the user **cannot** declare new **methods** outside the declaration **file** *(not even in the same **translation unit**)*
- ---
- **Problem:** ***initialization*** and ***deinitialization*** are not inheritable because they are binded to the ***constructor*** and ***destructor***
- **Solution:** differentiate ***initialization***/***deinitialization*** from  ***constructor***/***destructor***

{% tabs soft_objects00 %}
{% tab soft_objects00 class.h %}
```c
#pragma once

#define public    __attribute__((access(public)))
#define private   __attribute__((access(private)))
#define protected __attribute__((access(protected)))

#define class(CLASSNAME) \
  typedef struct CLASSNAME CLASSNAME; \
  struct CLASSNAME
```
{% endtab %}
{% tab soft_objects00 vehicle.h %}
```c
#pragma once

#include "class.h"

class(Vehicle) [[noinit, closed]] {
  int   wheels private;
  float weight protected;
};

// constructor
inline Vehicle *Vehicle::new(int wheels) {
  // Allocate
  Vehicle *v = malloc(sizeof(Vehicle));
  // Initialize (and acquire resources)
  v.__init__(wheels);
  // Return
  return v;
}

// destructor: operator~
inline Vehicle *(const Vehicle *v) __del__() {
  // Deinitialize (release resources)
  v.__deinit__();
  // Deallocate
  free(v);
  // Return
  return v;
}

// Mark as protected so children can call it, but outsiders do not.
// Inline makes it zero-cost for Vehicle::new or Car::new
inline void (Vehicle *v) __init__(int wheels) protected {
  v->wheels = wheels;
  v->weight = 0;
  // If needed, acquire resources
  // v->data = malloc(...)
  // ...
}

// Mark as protected so children can call it, but outsiders do not.
// Inline makes it zero-cost for Vehicle.__del__ or Car.__del__
inline void (Vehicle *v) __deinit__() protected {
  // Free any resource acquired in the constructor
  // free(v->data);
  // ...
}
```
{% endtab %}
{% tab soft_objects00 car.h %}
```c
#pragma once

#include "class.h"
#include "vehicle.h"

class(Car) [[noinit, closed]] {
  Vehicle;
  int doors;
};

// constructor
inline Car *Car::new(int doors, int wheels) {
  // Allocate
  Car *c = malloc(sizeof(Car));
  // Initialize (and acquire resources)
  c.__init__(doors, wheels);
  // Return
  return c;
}

// destructor: operator~
inline Car *(const Car *c) __del__() {
  // Deinitialize (release resources)
  c.__deinit__();
  // Deallocate
  free(c);
  // Return
  return c;
}

inline void (Car *c) __init__(int doors, int wheels) protected {
  // Call super initializer
  ((Vehicle*)c).__init__(wheels);

  // Initialize self
  c->doors = doors;
  c->weight = 1000.0f; // Overwrite protected "weight"

  // If needed, acquire resources
  // c->data = malloc(...)
  // ...
}

inline void (Car *c) __deinit__() protected {
  // Free any resource acquired in the constructor
  // free(c->data);
  // ...

  // Call super deinitializer
  ((Vehicle*)c).__deinit__();
}
```
{% endtab %}
{% tab soft_objects00 main.c %}
```c
#include "car.h"

int main() {
  Car *mycar = Car::new(5, 4);
  defer ~mycar; // destroy mycar at the end

  ...

  return 0;
}
```
{% endtab %}
{% endtabs %}

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
  _Alignas(8) char __reserved__; // reserve 8 bytes for private members
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
#include <stdlib.h>

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
Cat *(const Cat *c) __del__() {
  free(c);
  return c;
}

int main() {
  Cat *mycat = Cat::new("Mr. Pants", 3, 5.0, 10);
  defer ~mycat;

  mycat.grow();             // Call Cat.grow
  ((Animal*)&mycat).grow(); // Call Cat.grow

  return 0;
}
```
