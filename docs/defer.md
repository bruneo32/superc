---
title: Defer
layout: blog
---

# Defer
Will execute the code stated right before exiting a function

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

# Defer in loops
Will execute the code stated right before the break/continue label of a for/while/do loop.
At first glance it can look totally unnecessary, but for memory management scenarios can be very useful.

### Examples
```c
#include <stdio.h>

int main() {
  for (int i = 0; i < 3; i++) {
    defer printf("This will run when the functions is over. Nothing to do with the loop\n");
    defer break printf("[%d] This will run right before loop break\n", i);
    defer continue printf("[%d] This will run right before loop increment\n", i);
  }

  printf("OK\n");
  // [0] This will run right before loop increment
  // [1] This will run right before loop increment
  // [2] This will run right before loop increment
  // [3] This will run right before loop break
  // OK
  // This will run when the functions is over. Nothing to do with the loop
  return 0;
}
```

# Defer in scoped blocks
> **Warning**: Scoped blocks are not implemented yet, are in development phase

Sometimes is usefull to execute code after a scoped block, not only in loops.

### Examples
```c
#include <stdio.h>

int main() {
  int x = 8;

  {
    int *mem = malloc(sizeof(int));
    defer auto free(mem); // Will free 'mem' when the block is over, not the function
    *mem = 42 + x;
    printf("%d\n", *mem);
  }
  // mem is freed here

  return 0;
}
```
