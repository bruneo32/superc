---
title: (DRAFT) Default arguments in functions
layout: blog
---

# Default arguments in functions
Unlike C, **SuperC** allows parameters to have optional default values.
- If an argument is not provided, the default value is used instead of an error.

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

## Named arguments
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

{% tabs functions3 %}
{% tab functions3 SuperC %}
```cpp
#include <stdio.h>

#define C_BLACK 0x000000FF
#define C_WHITE 0xFFFFFFFF
#define C_RED   0xFF0000FF

Button Button::new(int background = C_BLACK, int color = C_WHITE,
                   const char *text = NULL, const char *tooltip = NULL);

int main() {
  /* Named arguments are great when a function has many parameters
   * with similar types.
   * They improve readability without requiring a struct,
   * which is less safe and can change the signature of the function */

  Button btn_play = Button::new(
    .text = "Play!",
    .tooltip = "Start a new adventure",
  );

  Button btn_quit = Button::new(
    .color = C_RED,
    .text = "Quit",
    .tooltip = "Exit the game",
  );

  ...

  return 0;
}
```
{% endtab %}

{% tab functions3 C99 %}
```c
#include <stdio.h>

#define C_BLACK 0x000000FF
#define C_WHITE 0xFFFFFFFF
#define C_RED   0xFF0000FF

struct ButtonParams {
  int background;
  int color;
  const char *text;
  const char *tooltip;
};

Button Button_new(ButtonParams params);

int main() {
  Button btn_play = Button_new((ButtonParams){
    .text = "Play!",
    .tooltip = "Start a new adventure",
  });

  Button btn_quit = Button_new((ButtonParams){
    .color = C_RED,
    .text = "Quit",
    .tooltip = "Exit the game",
  });

  ...

  return 0;
}
```
{% endtab %}
{% endtabs %}

## Collision with function overloading
Have in mind the following example, which `foo` gets called?
1. Overloads that do not require default arguments are preferred.
2. Overloads with fewer default arguments are preferred.

> The answer is `foo1`

```c
void foo(int a)            __attribute__((symbol("foo1")));
void foo(int a, int b = 0) __attribute__((symbol("foo2")));

int main() {
  foo((int)10);
  return 0;
}
```
