---
title: (DRAFT) Generics meta-programming
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Generics are currently **not implemented**. Syntax may change<br>

# Generics meta-programming
The compiler automatically duplicates and **generates** at compile-time all the *type-specific implementations* for the *template region* you defined as a **generic implementation template**.

- Generics operate purely at **compile-time** and do not introduce runtime overhead.
- Generics increases file size, and reduces symbol control. Use them when **type safety** and **flexibility** are paramount.

## Define template region
- Surround the *region* to duplicate with `#definefor T ...` and `#undef T`.
- Special **keywords** can be used as types:
  - `$type` for **automatic** type inference. This will look ahead to automatically determine the type parameters explicitly used in all the types, variables and functions in the template region.
  - `$signed` for **signed integer** types: `char, short, int, long, ...`
  - `$unsigned` for **unsigned integer** types: `unsigned char, unsigned short, unsigned int, unsigned long, ...`
  - `$integer` for **$signed+$unsigned** types: `char, unsigned char, short, unsigned short, ...`
  - `$floating` for **floating** point types: `float, double, long double, ...`
  - `$numeric` for **$integer+$floating** types: `char, unsigned char, float, short, unsigned short, double, ...`
- When writing ***generic libraries***, it may be interesting to use **[\_\_attribute\_\_((linkonce))](attributes#__attribute__linkonce)**.
  - For example, when compiling a *string library* against different *compile units*, the function `String::new` will be written to different **object files** (.o).
  `__attribute__((linkonce))` tells the linker to only keep one definition of `String::new` in the final executable.

{% tabs generics1 %}
{% tab generics1 SuperC %}
```c
#include <stdio.h>

// The compiler is going to repeat the emission of this region (template)
// for types `double` and `short`
#definefor T double, short

// The compiler emits two 'sum' functions
// - One replacing T with double:  double sum[double](double a, double b)
// - One replacing T with short:   short  sum[short](short a, short b)
T sum[T](T a, T b) {
  return a + b;
}

#undef T

int main() {
  printf("sum short:  %d\n", sum[short](1, 2));
  printf("sum double: %.2f\n", sum[double](3.14, 2.0));
  // sum short:  3
  // sum double: 5.14
  return 0;
}
```
{% endtab %}

{% tab generics1 C99 %}
```c
#include <stdio.h>

double sum__double(double a, double b) {
  return a + b;
}

short sum__short(short a, short b) {
  return a + b;
}

int main() {
  printf("sum short:  %d\n", sum__short(1, 2));
  printf("sum double: %.2f\n", sum__double(3.14, 2.0));
  // sum short:  3
  // sum double: 5.14
  return 0;
}
```
{% endtab %}
{% endtabs %}

## When Not to Use Generics
**Generics** programming is meant to be a last resource tool when there's no other option; or you want a more type safe, but more verbose solution.

Many cases can be covered with one of the following before using generics:
- **Macros**: simple type‑agnostic code *(e.g., `#define MAX(a,b) ((a)>(b)?(a):(b))`)*.
- **[Function overloading](function_overload.md)**: a few fixed types with different behaviour, more control over symbols.
- **void\*** + manual casting: low‑level or FFI code.

|                  | Macros | Overloads | Generics |
| ---------------- |:------:|:---------:|:--------:|
| Compile-time     | ✅     | ✅        | ✅       |
| Type safety      | ❌     | ✅        | ✅       |
| Flexible types   | ✅     | ❌        | ✅       |
| Code duplication | ❌     | ❌        | ⚠️       |
| Symbol control   | ❎\*   | ✅        | ⚠️       |

(*) **macros** are preprocessed so they don't have a symbol, but that's not bad.

## Symbol mangle
When a *type* is used inside **square brackets** next to an *identifier*, that makes it part of the *identifier*. So `math::sum[int]` is a **whole** *identifier*, just like `math_sum_int`.

To differentiate the assembly identifiers, the compiler uses ***"\$\$"*** for **namespaces** and ***".."*** for **generics**, so `math::sum[int]` will be ***"math\$\$sum..i"*** and does not conflict with `math::sum::int` (***"math\$\$sum\$\$int"***).

Note that unlike `::`, the compiler will transform the type inside square brackets to it's assembly type name ***(i.e., `int`=>`i`, `float`=>`f`, `char*`=>`Pc`, `struct Point[int]`=>`S8Point..i`, etc)***, so the symbol of `math::sum[int]` will be `math$$sum..i`.

```c
#include <stdio.h>

#definefor T int, float

T math::sum[T](T a, T b); // No problem
T math::sum::T(T a, T b); // Caution!

struct Point[T] { T x, y; };
void (struct Point[T] *this) add(T other);

void control::symbol[T](T a, T b) __attribute__((symbol("control_" T)));
void caution::symbol[T](T a, T b) __attribute__((symbol("caution_" #T)));

#undef T

// math::sum[int]   => "math$$sum..i"
// math::sum[float] => "math$$sum..f"
// math::sum::int   => "math$$sum$$int"
// math::sum::float => "math$$sum$$float"

// Caution! because "math$$sum$$float" has no problem, but
// something like: "math$$sum$$char*", would break the assembler or linker,
// or even worse: "math$$sum$$struct Point".

// point_i.add(1)   => "add$PS8Point..ii"
// point_f.add(1.0) => "add$PS8Point..ff"

// math::sum[struct Point[int]]   => "math$$sum..S8Point..i"
// math::sum[struct Point[float]] => "math$$sum..S8Point..f"

// control::symbol[int]   => "control_i"
// control::symbol[float] => "control_f"
// caution::symbol[int]   => "caution_int"
// caution::symbol[float] => "caution_float"

// Same warning as before, because "caution_int" won't have a problem,
// but "caution_char*" would break the assembler or linker.
```

## Examples
{% tabs generics3 %}

{% tab generics3 Vec2 %}
```c
#include <stdio.h>
#include <math.h>

/* Only allow Vec2[T] for numeric types ($integer+$floating) */
#definefor T $numeric

typedef struct Vec2[T] Vec2[T];
struct Vec2[T] { T x, y; };

inline Vec2[T] (Vec2[T] a) __add__(Vec2[T] b) {
  return (Vec2[T]){ a.x + b.x, a.y + b.y };
}

inline bool (Vec2[T] a) __eq__(Vec2[T] b) {
  return a.x == b.x && a.y == b.y;
}

#undef T

int main() {
  Vec2[float] a = {3.0f, 0.0f};
  Vec2[float] b = {0.0f, 4.0f};
  Vec2[float] s = a + b;
  printf("sum: %.1f %.1f\n", s.x, s.y);

  if (a + b == s)
    printf("equal\n");

  printf("OK\n");
  // sum: 3.0 4.0
  // equal
  // OK
  return 0;
}
```
{% endtab %}

{% tab generics3 Stack %}
```c
#include <stdio.h>
#include <stdlib.h>

// Automatic type inference, can be any type
#definefor T $type
typedef struct Stack[T] Stack[T];
struct Stack[T] {
  T* data;
  size_t top;
};

// constructor
inline Stack[T] Stack::new[T](const size_t cap) {
  return (Stack[T]){
    .data = malloc(sizeof(T) * cap),
    .top = 0
  };
}

// destructor: operator~
inline Stack[T] *(Stack[T] *s) __del__() {
  free(s->data);
  return s;
}

inline void (Stack[T] *s) push(T val) {
  s->data[s->top++] = val;
}

inline T (Stack[T] *s) pop() {
  return s->data[--s->top];
}

#undef T

int main() {
  Stack[int] *s = &Stack::new[int](10);
  defer ~s;

  s.push(28);
  s.push(100);
  s.push(42);
  printf("Popped: %d\n", s.pop());

  int a = s.pop();
  int b = s.pop();
  int c = a + b;
  printf("c: %d\n", c);

  printf("OK\n");
  // Popped: 42
  // c: 128
  // OK
  return 0;
}
```
{% endtab %}

{% tab generics3 Map %}
```c
#include <stdio.h>
#include <stdlib.h>

#definefor K $type
#definefor V $type

typedef struct MapEntry[K,V] MapEntry[K,V];
struct MapEntry[K,V] {
  K key;
  V val;
};

typedef struct Map[K,V] Map[K,V];
struct Map[K,V] {
  MapEntry[K,V] *buckets;
  size_t count;
  size_t capacity;
};

// constructor
inline Map[K,V] Map::new[K,V](const size_t initial_capacity = 256) {
  return (Map[K,V]){
    .buckets = (MapEntry[K,V]*)malloc(initial_capacity),
    .capacity = initial_capacity,
    .count = 0,
  };
}

// destructor: operator~
inline Map[K,V] (Map[K,V] m) __del__() {
  free(m.buckets);
  return m;
}

void (Map[K,V] *m) put(K key, V value) __attribute__((linkonce)) {
  /* If the map is full, double its capacity */
  size_t new_len = (m->count + 1) * sizeof(MapEntry[K,V]);
  if (new_len >= m->capacity) {
    while (new_len >= m->capacity)
      m->capacity *= 2;
    m->buckets = (MapEntry[K,V]*)realloc(m->buckets, m->capacity);
  }

  /* Insert the new entry */
  m->buckets[m->count++] = (MapEntry[K,V]){
    .key = key,
    .val = value,
  };
}

V (Map[K,V] m) get(K key) __attribute__((linkonce)) {
  for (size_t i = 0; i < m.count; i++) {
    if (m.buckets[i].key == key)
      return m.buckets[i].val;
  }
  // Not found
  return (V){0};
}

#undef V
#undef K

int main() {
  Map[char*,int] map1 = Map::new[char*,int]();
  defer ~map1;

  Map[int,float] map2 = Map::new[int,float]();
  defer ~map2;

  (&map1).put("foo", 123);
  (&map1).put("bar", 145);
  (&map1).put("baz", 987);

  (&map2).put(123, 32.0);
  (&map2).put(145, 3.1415);
  (&map2).put(987, 88.89);

  printf("bar: %d\n",   map1.get("bar"));
  printf("987: %.2f\n", map2.get(987));
  printf("Cross [foo]: %.2f\n", map2.get(map1.get("foo")));

  printf("OK\n");
  // bar: 145
  // 987: 88.89
  // Cross [foo]: 32.00
  // OK
  return 0;
}
```
{% endtab %}

{% tab generics3 More %}
```c
#include <stdio.h>

#include "stack.h"
#include "map.h"

#definefor K $type
#definefor V $type
/* Higher-order method */
inline void (Map[K,V] m) foreach(void (*callback)(K,V)) {
  for (size_t i = 0; i < m.count; i++)
    callback(m.buckets[i].key, m.buckets[i].val);
}
#undef V
#undef K

int main() {
  Stack[int] *s = &Stack::new[int](10);
  defer ~s;
  my_stack.push(99);

  Stack[Stack[int]] *stack_of_stacks = &Stack::new[Stack[int]](10);
  defer ~stack_of_stacks;

  stack_of_stacks.push(my_stack);
  int x = stack_of_stacks.pop().pop();
  printf("pop.pop: %d\n", x);

  Map[int,float] map = Map::new[int,float]();
  defer ~map;

  (&map).put(123, 32.0);
  (&map).put(145, 3.1415);
  (&map).put(987, 88.89);

  printf("--------\n");
  map.foreach(void (int k, float v) {
    // Call lambda for each entry in the map
    printf("%d -> %.2f\n", k, v);
  });
  printf("--------\n");

  printf("OK\n");
  // pop.pop: 99
  // --------
  // 123 -> 32.00
  // 145 -> 3.14
  // 987 -> 88.89
  // --------
  // OK
  return 0;
}
```
{% endtab %}
{% endtabs %}
