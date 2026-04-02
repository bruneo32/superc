---
title: (DRAFT) Namespaces
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Namespaces are currently **not implemented**. Syntax may change

# Namespaces
Big code projects must support namespaces for organization of big codebases. So **SuperC** will not survive without namespacing.

> Namespaces are not intended to reduce the amount of typing, but to **organize** the codebase and prevent **name collisions**.

But in order to keep it simple and avoid "namespace hell":
- **SuperC** allows `::` in **identifier** names *(but not single `:`, because that would break label parsing)*
- no `using namespace foo`, only **explicit** `foo::bar`
- no nesting namespaces, just a compile time **namespace prefix**
  - use `#pragma namespace(foo)` to tell the compiler to **prepend** `foo::` to all declaration identifiers
  - use `#pragma namespace(foo::bar)` to set the current prefix, instead of namespace nesting
  - use `#pragma namespace()` to **end** the current namespace (no prefix)
  - reaching the end of the file will **end** the current namespace too
- **default assembly symbol** mangling of `foo::bar` is `foo$bar`
  - this can be totally overriden with [attribute symbol](symbols.md#aliases)
  - this can be automated with `#pragma namespace_symbol("foo_")`, that will set the assembly prefix to `foo_*` instead of `foo$*`
  - each time you start or end a namespace (`#pragma namespace(*)`), the prefix is reset to **default**

> Note that `#pragma namespace` is a **SuperC** extension; **C** compilers will ignore it *(with a warning)*.

{% tabs namespaces1 %}
{% tab namespaces1 main.c %}
```cpp
#include <stdio.h>
#include "sdl_wrapper.h"

static SDL::Window   *game_window;
static SDL::Renderer *game_renderer;

int main() {
  /* Init SDL */
  if (SDL::Init(SDL_INIT_VIDEO) != 0)
    return 1;
  defer SDL::Quit();

  /* Create window */
  game_window = SDL::CreateWindow("Defer test",
                    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                    640, 480, SDL_WINDOW_SHOWN);
  if (!game_window)
    return 2;
  defer SDL::DestroyWindow(game_window);

  /* Create renderer */
  game_renderer = SDL::CreateRenderer(game_window, -1,
                    SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
  if (!game_renderer)
    return 3;
  defer SDL::DestroyRenderer(game_renderer);

  printf("OK\n");
  return 0;
}
```
{% endtab %}

{% tab namespaces1 sdl_wrapper.h %}
```cpp
#include <SDL2/SDL.h>

#pragma namespace(SDL)

typedef SDL_Window   Window;
typedef SDL_Renderer Renderer;

// default symbol is "SDL$Init", so make an alias to SDL_Init
extern DECLSPEC int SDLCALL Init(Uint32 flags)
        __attribute__((symbol("SDL_Init")));

extern DECLSPEC void SDLCALL Quit(void)
        __attribute__((symbol("SDL_Quit")));

// Tired of typing __attribute__((symbol("SDL_*"))?
// Let's make it easier by changing the
// default assembly prefix
#pragma namespace_symbol("SDL_")

extern DECLSPEC SDL_Window * SDLCALL CreateWindow(const char *title,
                                                  int x, int y, int w,
                                                  int h, Uint32 flags);

extern DECLSPEC SDL::Renderer * SDLCALL CreateRenderer(SDL::Window * window,
                                                       int index, Uint32 flags);

extern DECLSPEC void SDLCALL DestroyWindow(SDL_Window * window);
extern DECLSPEC void SDLCALL DestroyRenderer(SDL::Renderer * renderer);

// no need for #pragma namespace(), since pragma only last for the current file
```
{% endtab %}
{% endtabs %}

## Optional alias to reduce namespace nesting
You can use a **macro** to reduce deep nested namespaces in some cases.

```cpp
#define Algebra Math::Algebra::Arithmetic
printf("%d\n", Arithmetic::Add(1, 2)); // equivalent to Math::Algebra::Arithmetic::Add
```

> Note that you can reduce nested namespaces to one namespace (**Arithmetic::Add**`), but no to zero (**Add**).
