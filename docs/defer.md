---
title: Defer
layout: blog
---

# Defer
Will execute the code stated right before exiting a function.

- Very useful for *dynamic memory management*, *resource cleanup* and *error handling*.
- Easier to read the program since all the code for the cleanup is near the declaration.
- Defers are executed in **LIFO** order. **i.e.**, the last defer will be executed first.
- It is **encouraged** to use defer when possible, since it's generally safer and faster. (See [assembly code generation](#assembly-code-generation))

### Examples
{% tabs defer1 %}
{% tab defer1 SuperC %}
```cpp
#include <stdio.h>

int main() {
  defer printf("This will print after OK\n");
  printf("OK\n");
  return 0;
}
```
{% endtab %}

{% tab defer1 Output %}
```
OK
This will print after OK
```
{% endtab %}
{% endtabs %}

{% tabs defer2 %}
{% tab defer2 SuperC %}
```cpp
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

  /* Write to file test.txt */
  fprintf(f, "Hello world!\n");

  printf("OK!\n");
  return 0;
}
```
{% endtab %}

{% tab defer2 Output 1 %}
**test.txt**:
```
Hello world!
```
**Output**:
```
OK!
File closed!
```
{% endtab %}

{% tab defer2 Output 2 %}
**test.txt**:
```
```
**Output**:
```
No need to call fclose(), because the defer executes before the return.
File closed!
```
{% endtab %}
{% endtabs %}

{% tabs defer3 %}
{% tab defer3 SuperC %}
```cpp
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
   * This example shows a basic usage of defer in a real scenario.
   * If function returns early, it will execute the defer statements before
   * exiting the function.
   * i.e.:
   *   - If game_renderer is NULL, SDL_DestroyWindow and SDL_Quit will be called
   *     (in that order, reverse to declaration), but SDL_DestroyRenderer won't.
   *     Because the return happens before the defer is declared.
   */

  printf("OK\n");
  return 0;
}
```
{% endtab %}
{% endtabs %}

{% tabs defer4 %}
{% tab defer4 SuperC %}
```cpp
#include <stdio.h>

int main() {
  /*
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
  // OK
  // Actual defer execution
  return 0;
}
```
{% endtab %}

{% tab defer4 Output %}
```
OK
Actual defer execution
```
{% endtab %}
{% endtabs %}

## Assembly code generation
Defer in **SuperC** is not a function call, or a rewriting the code before each return, it is **simpler** and **clever** than that.

- This implementation makes defer works as **zero-cost overhead** because it's just natural program flow, no hidden function calls.
- This implementation **avoids code duplication** by jumping to the deferred code. No rewriting code at each `return`.

Below is a pseudo assembly example of how the **SuperC** compiler emits defer statements.

> Can you guess what `x` will be at the end of the program?<br>
> **a.** 10<br>
> **b.** 99<br>
> **c.** 20<br>
> **d.** stack garbage

{% tabs defer-ass %}
{% tab defer-ass SuperC %}
```cpp
#include <stdio.h>

int main() {
  int x;
  defer printf("x: %d\n", x);

  x = 10;

  if (x < 10)
    return 1;

  defer {
    x = 99;
  }

  x = 20;

  return 0;
}
```
{% endtab %}

{% tab defer-ass pseudo-assembly %}
```s
.LC0:
  .string "x: %d\n"
  .text
  .globl main
  .type main, @function

main:
  # -- function begin --
  push rbp
  mov rbp, rsp
  sub rsp, 24

  # -- function body --

  # x = 10
  mov DWORD PTR -20[rbp], 10

  # if (x < 10)
  cmp DWORD PTR -20[rbp], 10
  jge .L2 # --> jump to .L2 if (x >= 10)
  # return 1
  push DWORD 1
  jmp .__defer0

.L2:
  # x = 20
  mov DWORD PTR -20[rbp], 20

  # return 0
  push DWORD 0
  jmp .__defer1 # Optimized out

.__defer1:
  # x = 99
  mov DWORD PTR -20[rbp], 99

.__defer0:
  # printf("x: %d\n", x)
  mov eax, DWORD PTR -20[rbp]
  mov esi, eax
  lea rax, .LC0[rip]
  mov rdi, rax
  mov eax, 0
  call printf@PLT

  # -- function end --
  # return code is in eax
  pop eax  # restore the return code
  leave
  ret
```
{% endtab %}

{% tab defer-ass pseudo-c %}
```c
#include <stdio.h>

int main() {
  int __retval__;

  int x;
  // register __defer0

  x = 10;

  if (x < 10) {
    // return 1;
    __retval__ = 1;
    // if program reaches this goto, it's jumping to __defer0
    // so it skips __defer1 because it has not been registered yet
    goto __defer0;
  }

  // register __defer1

  x = 20;

  // return 0;
  __retval__ = 0;
  goto __defer1;

  __defer1:
  x = 99;

  __defer0:
  printf("x: %d\n", x);

  return __retval__;
}
```
{% endtab %}

{% tab defer-ass Output %}
```
x: 99
```
{% endtab %}
{% endtabs %}
