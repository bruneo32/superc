---
title: (DRAFT) &#35;definefor macro
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. &#35;definefor are currently **not implemented**. Syntax may change<br>

# &#35;definefor macro
The *preprocessor* will **duplicate** the region, **substituting** the macro **name** with each one of the **values** listed.

> Note that this has been created primarly as a workaround to implement [generics meta-programming](generics.md), but it can be used for other purposes.

## Define template region
- Surround the *region* to duplicate with `#definefor NAME ...` and `#undef NAME`.

{% tabs definefor1 %}
{% tab definefor1 SuperC %}
```c
#include <stdio.h>

// The compiler is going to repeat the emission of
// this region for values 1, 2, 3, and 4
#definefor N 1,2,3,4

int var::N = N;
int fn::N() { return var::N; }

#undef N // end of region

int main() {
  printf("var 3: %d\n", fn::3());
  // var 3: 3
  return 0;
}
```
{% endtab %}

{% tab definefor1 C99 %}
```c
#include <stdio.h>

int var__1 = 1;
int fn__1() { return var__1; }

int var__2 = 2;
int fn__2() { return var__2; }

int var__3 = 3;
int fn__3() { return var__3; }

int var__4 = 4;
int fn__4() { return var__4; }

int main() {
  printf("var 3: %d\n", fn__3());
  // var 3: 3
  return 0;
}
```
{% endtab %}
{% endtabs %}
