---
title: Namespaces
layout: blog
---

> ⚠️ Namespace are implemented **[up-stream](https://github.com/bruneo32/superc){:target="_blank"}**, but the **playground** does not support it yet

# Namespaces
Big code projects must support namespaces to organize large codebases. So **SuperC** will not survive without namespacing.

> Namespaces are **not** intended to **reduce** the amount of typing, but to **organize** the code and prevent **name collisions**.

But in order to keep it simple and avoid "namespace hell":
- Identifiers do not track namespace metadata. Instead, `::` is treated as part of the identifier name.
- The default assembly symbol for `foo::bar` is `foo$$bar`.
- There is no implicit `using namespace foo`; only **explicit** usage like `foo::bar`.
- Nested namespaces are not supported. Instead, a compile-time **namespace prefix** is used (see [#pragma namespace](#pragma-namespace)).

## Identifier names
**SuperC** allows the use of `::` inside **identifier** names *(but not single character `:`, as that would conflict with label parsing)*

- Valid **SuperC** identifiers: `foo`, `foo::bar`, `foo::bar::baz`, etc.
- Invalid **SuperC** identifiers: `foo:::bar`, `foo:bar`, etc.

---
- Only **global** variables/functions/typedefs are eligible to be namespaced.
- **Local** variables/lambdas/typedefs cannot be namespaced, and will throw an error.
- **Type methods** cannot be namespaced, and will throw an error.

## \#pragma namespace
Instead of constantly typing `foo::` before all the identifiers in your header files, you can use `#pragma namespace foo`, this assigns the ***name prefix*** to `foo::`.
- use `#pragma namespace foo` to tell the compiler to **prepend** `foo::` to all eligible identifiers names.
- use `#pragma namespace` to **end** the current namespace. **i.e.,** set *name prefix* to empty.
- reaching the end of the file will also **end** the current namespace.
- after an `#include` directive, the *name prefix* is **restored** to the previous one, preventing the included file from hoisting the *name prefix*.
- [type methods](methods.md) are **not** affected by the *name prefix*.

Same as *name prefix*, there is also a ***symbol prefix*** *(see [\#pragma namespace_symbol](#pragma-namespace_symbol))*
- each time `#pragma namespace` is reached, the *symbol prefix* is **reset**. **i.e.,** `#pragma namespace foo` makes the *symbol prefix* `foo$$` and the *name prefix* `foo::`.
- this *default symbol* can be completely overridden with a [attribute symbol](symbols.md#aliases)

> **Note**: the `#pragma namespace` directive replaces the previous one. If you invoke `#pragma namespace foo` and then `#pragma namespace bar`, the namespace is not `foo::bar`, it's `bar`. Use `#pragma namespace foo::bar` to indicate a nested namespace.

## \#pragma namespace_symbol
- You can use `#pragma namespace_symbol "foo_"` to assign the ***symbol prefix*** to *"foo_"* instead of the default `foo$$`.
- `#pragma namespace_symbol ""` effectively disables the *symbol prefix*, useful for wrappers.
- after an `#include` directive, the *symbol prefix* is **restored** to the previous one, preventing the included file from hoisting the *symbol prefix*.
- Note that when the `#pragma namespace` directive is reached, the *symbol prefix* is **reset** to default. See [\#pragma namespace](#pragma-namespace)

## Reduce deep namespace identifiers
You can use a **macro** to reduce deep nested namespaces in some cases.
```cpp
#define Arithmetic Math::Algebra::Arithmetic
printf("%d\n", Arithmetic::Add(1, 2)); // equivalent to Math::Algebra::Arithmetic::Add
```

## Examples

{% tabs namespaces1 %}
{% tab namespaces1 example1.c %}
```cpp
#include <stdio.h>

// no namespace
int sum(int a, int b) {
  return a + b;
}

// explicit namespace
int mylib::sum(int a, int b) {
  return a + b + 1;
}

/* -- BEGIN NAMESPACE: mylib -- */
#pragma namespace mylib
const int TWO = 2;       // symbol : mylib$$TWO
int add(int a, int b) {  // symbol : mylib$$add
  return a + b + mylib::TWO;
}

// methods don't get namespace, so this is
// not '(int).mylib::sum', but just '(int).sum'
int (int a) sum(int b) {
  return a + b;
}

#pragma namespace // end namespace

int main() {
  int x = 3;
  printf("A: %d\n", sum(1, 2));
  printf("B: %d\n", mylib::sum(1, 2));
  printf("C: %d\n", mylib::add(1, 2));
  printf("D: %d\n", x.sum(4)); // (int).sum
  // A: 3
  // B: 4
  // C: 6
  // D: 7
  return 0;
}
```
{% endtab %}

{% tab namespaces1 example2.c %}
```cpp
#include <stdarg.h>

#pragma namespace std
#pragma namespace_symbol "" // disable symbol mangling
// wrap all std headers inside `std::`
#include <stdio.h>
#include <stdlib.h>
#pragma namespace // end namespace

// warning: 'std::printf' has symbol "printf",
// so change the symbol of this function to avoid collision
void printf(const char *format, ...) __attribute__((symbol("myprintf"))) {
  va_list args;
  va_start(args, format);
  std::puts("Hi: ");
  std::vprintf(format, args);
  va_end(args);
}

int main() {
  printf("Hello world\n"); // output: "Hi: Hello world\n"
  std::printf("OK\n");     // the classic printf from <stdio.h>
  return 0;
}
```
{% endtab %}

{% tab namespaces1 example3.c %}
```cpp
#include <stdio.h>
#include "sdl_wrapper.h"

int main() {
  /* Init SDL */
  if (SDL::Init(SDL_INIT_VIDEO) != 0)
    return 1;
  defer SDL::Quit();

  /* Create window */
  SDL::Window *game_window = SDL::CreateWindow("Defer test",
                    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                    640, 480, SDL_WINDOW_SHOWN);
  if (!game_window)
    return 2;
  defer SDL::DestroyWindow(game_window);

  ...

  printf("OK\n");
  return 0;
}
```
{% endtab %}

{% tab namespaces1 sdl_wrapper.h %}
```cpp
#include <SDL2/SDL.h>

#pragma namespace SDL
#pragma namespace_symbol "SDL_"
// SDL::Init -> SDL_Init
// SDL::Quit -> SDL_Quit
// ...

typedef SDL_Window   Window;
typedef SDL_Renderer Renderer;

extern DECLSPEC int SDLCALL Init(Uint32 flags);
extern DECLSPEC void SDLCALL Quit(void);

extern DECLSPEC SDL::Window * SDLCALL CreateWindow(const char *title,
                                                   int x, int y, int w,
                                                   int h, Uint32 flags);
extern DECLSPEC void SDLCALL DestroyWindow(SDL::Window * window);

// namespace will automatically end at the end of the file
```
{% endtab %}
{% endtabs %}
