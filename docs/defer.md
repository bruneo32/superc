---
title: Defer
layout: blog
---

# Defer
Will execute the code stated right before exiting a function.

It is encouraged, because it does not produce duplicated code, and does not create function calls.

> Note: Defers are executed in LIFO order. **i.e.**, the last defer will be executed first.

### Examples
```c
#include <stdio.h>

int main() {
  defer printf("This will print after OK\n");
  printf("OK\n");
  // OK
  // This will print after OK
  return 0;
}
```
```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  srand(time(NULL));

  FILE *f = fopen("test.txt", "w");
  defer {
    fclose(f);
    printf("File closed!\n");
  }

  /* 50% chance to exit early with error */
  if ((rand() & 1) == 0) {
    printf("No need to call fclose(), because the defer executes before the return.\n");
    return 1;
  }

  fprintf(f, "Hello world!\n");

  return 0;
}
```
```c
#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

static SDL_Window   *game_window;
static SDL_Renderer *game_renderer;

int main() {
  /* Init SDL */
  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    return 1;
  defer SDL_Quit();

  /* Create window */
  game_window = SDL_CreateWindow("Defer test",
                    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                    640, 480, SDL_WINDOW_SHOWN);
  if (!game_window)
    return 2;
  defer SDL_DestroyWindow(game_window);

  /* Create renderer */
  game_renderer = SDL_CreateRenderer(game_window, -1,
                    SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
  if (!game_renderer)
    return 3;
  defer SDL_DestroyRenderer(game_renderer);

  /*
   * This example shows a basic usage of defer.
   * If function returns early, it will execute the defer statements before
   * exiting the function.
   * i.e.:
   *   - If game_renderer is NULL, SDL_DestroyWindow and SDL_Quit will be called
   *     (in that order, reverse to declaration), but SDL_DestroyRenderer won't.
   *     Because the return happens before the defer is declared.
   *
   * [!!!]
   * Beware that defer statements are related to the function, not the block of code.
   * This means that even though the following if statement never runs,
   * the following defer is declared and will be executed because `main`
   * does not return before the defer is declared.
   */
  if (1 == 0) {
    defer printf("Actual defer execution\n");
    return 4;
  }

  printf("OK\n");
  return 0;
}
```

# Defer in scoped blocks
> **Warning**: `defer auto` is **not implemented yet**, it is in development phase

Sometimes is usefull to execute code after a scoped block, not only in loops.

This can be done using `defer auto`, it will always execute the deferred code even if there's a branching code like `goto`.

This also applies for [break n](break_n.md).

### Examples
```c
#include <stdio.h>

int main() {
  int x = 8;

  {
    int *mem = malloc(sizeof(int));
    defer auto {
      free(mem);
      printf("mem is freed here\n");
    }

    *mem = 42 + x;
    printf("%d\n", *mem);
  }

  printf("OK\n");

  // 50
  // mem is freed here
  // OK
  return 0;
}
```
```c
#include <stdio.h>

int main() {
  int x = 8;

  {
    defer auto printf("I am unavoidable\n");
    // Try to escape defer execution
    goto lbl_end;
    printf("Unreachable\n");
  }

  lbl_end:
  printf("OK\n");

  // I am unavoidable
  // OK
  return 0;
}
```
```c
#include <stdio.h>
#include <stdbool.h>

int main() {
  while (true) {
    defer auto printf("A\n");
    while (true) {
      defer auto printf("B\n");
      while (true) {
        defer auto printf("C\n");
        // Exit 3 loops
        // Executing defers in LIFO order
        break 3;
      }
    }
  }

  printf("OK\n");
  // C
  // B
  // A
  // OK
  return 0;
}
```

# Defer in loops
> **Warning**: `defer auto` is **not implemented yet**, it is in development phase

In addition, `defer auto` has a special behavior in loops.

It runs also before the `continue`/`break` statements. So the defer statement is executed at the end of every iteration.

### Examples
```c
#include <stdio.h>

int main() {

  for (int i = 0; i < 5; i++) {
    char *str = malloc(100); // 100 chars max
    defer auto free(str);

    if (i == 1)
      continue; // defer runs here

    sprintf(str, "Number: %d", i);
    printf("%s\n", str);

    if (i == 3)
      break; // defer runs here

    // defer runs here
  }

  printf("OK\n");

  // Number: 0
  // Number: 2
  // Number: 3
  // OK
  return 0;
}
```
