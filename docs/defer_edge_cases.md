---
title: (DRAFT) Defer edge cases
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Defer is **semi-implemented**<br>
> Development is not going to advance until [LLVM backend](/#roadmap) is minimally stable.

# Possible cancellation points for defer statements
> This is still up to debate, we have covered ***goto***, but we have not talked about ***[longjmp](<https://gist.github.com/MangaD/b10d6a4c50e80712bc582e5968ae8bd0>)***, ***_Noreturn*** functions or ***signals***.

The current idea *(and this can be changed)* is that the user *may* or *may not* want to run defers at some cancellation points, because maybe the user wants to fast-fail, *among other reasons*.

Also, some cancellation points are *"easy"* to catch, but some are **impossible**, like [signals](<https://www.geeksforgeeks.org/c/signals-c-language/>){:target="_blank"}.

Nowadays, there are two ideas to leverage, that can coexists:
- **Builtin unwind defers**: Use a builtin function that automatically unwinds the defers. **i.e.**, `__builtin_unwind_defers()`.
  - **Pros**: no need to know the order of the defers.
  - **Cons**: harder to debug.
- **Assign defer statement**: Let the user assign the defer statement to a variable `auto defer_c = defer printf("C");`, this registers the defer as a [closure](lambdas.md#closures), then and call it when necessary `defer_c();`
  - **Pros**: more control, can call a defer anywhere, can pass the defer to another function, etc.
  - **Cons**: may explode the program if poorly used.
  - **Possible fixes**:
    - Throw a **compiler warning** when control flow skipes over a defer, hinting to call it.

> Since both ideas are non-exclusive, we may implement **assign defer statement**, and make `__builtin_unwind_defers()` to just write the closure calls in order.

{% tabs defer_edge_cases1 %}
{% tab defer_edge_cases1 builtin unwind defers %}
```c
#include <stdio.h>

_Noreturn void panic(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

int main() {
  defer printf("A\n");

  if (fatal) {
    defer auto printf("B\n");

    __builtin_unwind_defers();
    panic("fatal error");
  }

  return 0;
}
```
{% endtab %}

{% tab defer_edge_cases1 assign defer statement %}
```c
#include <stdio.h>

_Noreturn void panic(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

int main() {
  auto defer_a = defer printf("A\n");

  if (fatal) {
    auto defer_b = defer auto printf("B\n");

    defer_b();
    defer_a();
    panic("fatal error");
  }

  return 0;
}
```
{% endtab %}
{% endtabs %}

## Edge case code generation
{% tabs defer_auto1 %}
{% tab defer_auto1 Pseudo-C %}

```c
#include <stdio.h>
#include <stdbool.h>

/* Written lambdas instead of closures for simplicity */
__attribute__((fastcall))
static void defer_z() {
  printf("Z\n");
}
__attribute__((fastcall))
static void defer_a() {
  printf("A\n");
}
__attribute__((fastcall))
static void defer_b() {
  printf("B\n");
}
__attribute__((fastcall))
static void defer_c() {
  printf("C\n");
}

int main() {
  char x; // stack garbage

  // defer printf("Z\n");

  while (true) {
    lbl_break2_special: ;

    // defer auto printf("A\n");

    while (true) {
      // type inference
      // defer auto printf("B\n");

      while (true) {
        // defer auto printf("C\n");

        if (x == 7) {
          // -> run defer C
          // -> run defer B
          // -> run defer A
          defer_c();
          defer_b();
          defer_a();
          goto lbl_break3;
        } else if (x > 100) {
          // -> run defer C
          // -> run defer B
          defer_c();
          defer_b();
          // do not call defer A because lbl_break2
          // is inside the scope of defer A
          goto lbl_break2;
        } else if (x == -1) {
          // -> run defer C
          // -> run defer B
          // -> run defer A
          defer_c();
          defer_b();
          defer_a();
          // This one calls defer A,
          // because it jumps before the defer registration;
          // effectively, out of scope of defer A
          goto lbl_break2_special;
        } else if (!x) {
          // funnel this scope-level
          // -> run defer C
          // -> run defer B
          // -> run defer A
          defer_c();
          defer_b();
          defer_a();
          // funnel function-level
          // -> run defer Z
          defer_z();
          exit(1);
        } else {
          // funnel this scope-level
          // -> run defer C
          // -> run defer B
          // -> run defer A
          defer_c();
          defer_b();
          defer_a();
          return 1; // -> will run defer Z
        }

        // -> run defer C
        { printf("C\n"); }
      }

      if (x == 7) {
        // -> run defer B
        // -> run defer A
        defer_b();
        defer_a();
        goto lbl_break3;
      }

      // -> run defer B
      { printf("B\n"); }
    }

    lbl_break2:

    if (x == 7) {
      // -> run defer A
      defer_a();
      goto lbl_break3;
    }

    // -> run defer A
    { printf("A\n"); }
  }

  lbl_break3:

  return 0;
  // -> run defers Z after any return label
}
```

{% endtab defer_auto1 %}
{% endtabs %}

### Optimizations
Some possible optimizations to this approach are:
- If the defer statement is only a simple statement, like arithmetic, it can be inlined.
- If the defer statement is only a function call, we might register this function call instead of the closure.
- If the defer is never used early *(i.e., no goto, no break, etc)*, don't need to emit a closure, just write the defer statement in the program flow ***(normal [defer](defer.md#assembly-code-generation) execution)***
- To reduce code duplication, if a defer is used multiple times, we can call the closure too in the normal execution path instead of inlining it.
