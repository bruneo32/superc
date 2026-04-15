---
title: (DRAFT) Default arguments in functions
layout: blog
---

# Default arguments in functions
Unlike C, **SuperC** allows an optional *default value* for a parameter when no argument is provided.
{% tabs functions1 %}
{% tab functions1 SuperC %}
```cpp
#include <stdio.h>

int add(int a, int b = 1) {
  // If b is not provided, b = 1
  return a + b;
}

int main() {
  printf("2 + 1 = %d\n", add(2));
  printf("2 + 3 = %d\n", add(2, 3));
  printf("OK\n");
  // 2 + 1 = 3
  // 2 + 3 = 5
  // OK
  return 0;
}
```
{% endtab %}

{% tab functions1 C99 %}
```c
#include <stdio.h>

int add(int a, int b) {
  return a + b;
}

int main() {
  printf("2 + 1 = %d\n", add(2, 1));
  printf("2 + 3 = %d\n", add(2, 3));
  printf("OK\n");
  // 2 + 1 = 3
  // 2 + 3 = 5
  // OK
  return 0;
}
```
{% endtab %}
{% endtabs %}

## Select parameters by name
When a parameter has a *default value*, it can be selected by **name** instead of position.
> Note that the order of the arguments does not matter when they are selected by name.

{% tabs functions2 %}
{% tab functions2 SuperC %}
```cpp
#include <stdio.h>

struct Point {
  float x;
  float y;
};

struct Point new_point(float px = 0, float py = 0) {
  return (struct Point){.x = px, .y = py};
}

int main() {
  struct Point p1 = new_point();       // x:0, y:0
  struct Point p2 = new_point(1, 2);   // x:1, y:2
  struct Point p3 = new_point(.py=3);  // x:0, y:3
  struct Point p4 = new_point(.py=5,   // x:7, y:5
                              .px=7);
  return 0;
}
```
{% endtab %}

{% tab functions2 C99 %}
```c
#include <stdio.h>

struct Point {
  float x;
  float y;
};

struct Point new_point(float px, float py) {
  return (struct Point){.x = px, .y = py};
}

int main() {
  struct Point p1 = new_point(0, 0);  // x:0, y:0
  struct Point p2 = new_point(1, 2);  // x:1, y:2
  struct Point p3 = new_point(0, 3);  // x:0, y:3
  struct Point p4 = new_point(7, 5);  // x:7, y:5
  return 0;
}
```
{% endtab %}
{% endtabs %}
