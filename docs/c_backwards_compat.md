---
layout: blog
title: C backwards compatibility
---

# SuperC is a superset of C
What does it mean that **SuperC** is a superset of **[C11](<https://en.wikipedia.org/wiki/C11_(C_standard_revision)>)**{:target="_blank"}?

**SuperC** adds new rich features to C without breaking compatibility with C11.
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
