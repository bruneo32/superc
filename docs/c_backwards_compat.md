---
layout: blog
title: C backwards compatibility
---

# SuperC is a superset of C
What does it mean that **SuperC** is a superset of **[C11](<https://en.wikipedia.org/wiki/C11_(C_standard_revision)>)**{:target="_blank"}?

**SuperC** adds new rich features to C, without breaking compatibility with **C11**.
- **SuperC** can compile old **C** code without problems
- But regular **C** compilers will not compile **SuperC** code.

## Differences with C
Even though **SuperC** compile **C** code, there are some changes made on purpose over the **C** spec.

### Inline functions
#### In C99
`inline` keyword does not always make the function inlined, but rather tells the compiler to decide when to inline the function. In order to make it always inline you would have to use `__attribute__((always_inline))`.

As a result of this defensive definition, most `inline` functions are never inlined, which may be unexpected and unintuitive for *non-battle-scarred* C programmers.

> <https://en.wikipedia.org/wiki/Inline_(C_and_C++)#Storage_classes_of_inline_functions>

---

#### In SuperC
`inline` keyword makes the function inlined **always**, because it's the user decision to make it inline or not.

If we don't want it to be inlined, we can just remove `inline` from the function declaration.

{% tabs scdiffc1 %}
{% tab scdiffc1 SuperC %}
```c
#include <stdio.h>

// both functions are inlined at assembly level,
// effectively making them macros

inline add(int a, int b) {
  return a + b;
}

static inline sum(int a, int b) {
  return a + b;
}

int main() {
  add(2, 3);
  sum(4, 5);
  return 0;
}
```
{% endtab %}

{% tab scdiffc1 c99 %}
```c
#include <stdio.h>

// maybe inlined
inline add(int a, int b) {
  return a + b;
}

// Not inlined, surprise! Disassemble the binary to know
// try __attribute__((always_inline)) to fix it.
static inline sum(int a, int b) {
  return a + b;
}

int main() {
  add(2, 3);
  sum(4, 5);
  return 0;
}
```
{% endtab %}
{% endtabs %}

### \_\_func\_\_ and \_\_FUNCTION\_\_
Like [GCC](<https://gcc.gnu.org/onlinedocs/gcc/Function-Names.html>){:target="_blank"}, **SuperC** supports the `__func__`/`__FUNCTION__` macros, which returns the name of the current function.

- But SuperC has type methods and symbol mangling, so what should **\_\_FUNCTION\_\_** return in this cases?
- For compatibility, **\_\_FUNCTION\_\_** returns the [symbol](symbols.md) of the function, which is the name of the function in regular **C** functions.

{% tabs scdiffc2 %}
{% tab scdiffc2 SuperC %}
```c
#include <stdio.h>

int add(int a, int b) __attribute__((symbol("myadd_function"))) {
  printf("%s(%d, %d)\n", __FUNCTION__, a, b);
  return a + b;
}

// automatic symbol is "sum$ii"
int (int a) sum(int b) {
  // __func__ is exactly the same as __FUNCTION__
  printf("%s(%d, %d)\n", __func__, a, b);
  return a + b;
}

int main() {
  printf("%s:%d -> %s()\n", __FILE__, __LINE__, __FUNCTION__);
  // myfile.c:15 -> main()

  add(5, 10);
  // myadd_function(5, 10)

  ((int)10).sum(5);
  // sum$ii(10, 5)

  return 0;
}
```
{% endtab %}
{% endtabs %}
