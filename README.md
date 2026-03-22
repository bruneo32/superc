# SuperC: C11 superset language

A backwards-compatible extension of **C** with modern ergonomics like *defer*, *methods*, and *operator overloading*, without sacrificing performance and control.

> ⚠️ **Alpha**: expect bugs, missing features, and breaking changes.

[![License](https://img.shields.io/github/license/bruneo32/superc)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-gh--pages-blue)](https://bruneo32.github.io/superc/)

### Index

- [Why SuperC?](#why-superc)
- [Features](#features)
- [Quick start](#quick-start)
- [Roadmap](#roadmap)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [Notes](#notes)

# Why SuperC?
> **C** is powerful, but unforgiving. **SuperC** keeps that power, while making it easier to use correctly.

**SuperC** does not aim to replace existing high-level languages, but rather to be a very good option for [embedded systems](<https://en.wikipedia.org/wiki/Embedded_system#Characteristics>) programming. Hence, making it a great option for **general purpose** development too.

- **Zero‑cost abstractions** - sometimes even more efficient than hand-written C.
- **No runtime overhead** - no hidden allocations, no vtables.
- **Full C compatibility** - use any existing C library without wrappers.
- **Multiplatform** - write once, run on Linux, Windows, macOS with minimal `#ifdef`.
- **Great documentation** - tutorials, examples, and edge cases all organized.<br>No need to spend hours reading random forum posts or blindly pasting AI responses.

> Read more about vision and scope [here](<https://bruneo32.github.io/superc/#what-is-superc>).

# Features
- Current developed features
  - [Defer](<https://bruneo32.github.io/superc/docs/defer>)
  - [Type methods](<https://bruneo32.github.io/superc/docs/methods>)
  - [Symbol mangling](<https://bruneo32.github.io/superc/docs/symbols>)
    - [Aliases](<https://bruneo32.github.io/superc/docs/symbols#aliases>)
  - [break N](<https://bruneo32.github.io/superc/docs/break_n>)
  - [Operator overload](<https://bruneo32.github.io/superc/docs/operator_overload>)
  - [Function overload](<https://bruneo32.github.io/superc/docs/function_overload>)
- Current planned features
  - *Struct inheritance/composition*
  - [Defer auto (scoped blocks)](<https://bruneo32.github.io/superc/docs/defer_auto>)
    - [Defer in loops](<https://bruneo32.github.io/superc/docs/defer_auto#defer-in-loops>)
  - [Lambdas](<https://bruneo32.github.io/superc/docs/lambdas>)
    - [Closures](<https://bruneo32.github.io/superc/docs/lambdas#closures>)

# Quick start
Right now the compiler is not reliable enough to showcase, but sure you can try to test the new features.
> ⚠️ **Reminder**: the compiler is unstable right now. **Expect bugs**.

## Installation
```sh
# Clone
git clone https://github.com/bruneo32/superc.git
cd superc
# Build
make
# Setup compiler
alias superc="${PWD}/superc -I${PWD}/include "
# Build examples
superc example_simple.c
superc example_string.c
```

### Examples
example_simple.c
```c
#include <stdio.h>

int main() {
  FILE *f = fopen("example_simple.c", "r");
  defer {
    fclose(f);
    printf("File closed!\n");
  };

  printf("OK\n");
  // OK
  // File closed!
  return 0;
}
```

example_string.c
```c
#include <stdio.h>
#include <string.h>

/* Silly GCC headers
 * destroy __attribute__ */
#ifdef __attribute__
#undef __attribute__
#endif

#define string char*

/* s1.length() is strlen(s1) */
inline size_t (string s1) length() {
  return strlen(s1);
}

/* s1 += s2 is strcat(s1, s2) */
string (string s1) __iadd__(const string s2) __attribute__((symbol("strcat")));

int main() {
  char s1[100] = "Hello, ";
  s1 += "world!"; // string += string

  printf("(%d) = %s\n", s1.length(), s1);
  // (13) = Hello, world!
  return 0;
}
```

# Roadmap
- **Documentation**
  - Keep [documentation](<https://bruneo32.github.io/superc/>) up-to-date.
  - Provide dual-track explanations: by-the-hand guides for beginners, and straight forward documentation for experienced developers.
  - Use examples *(at least the following)*: simple usage, real usage, and edge cases.
- **Compiler**
   1. Complete **LLVM** backend
      - Right now, the backend is **experimental** x86_64 specific, unoptimized assembly. Just meant to showcase the new language features.
   2. Complete **[C11](<https://en.wikipedia.org/wiki/C11_(C_standard_revision)>)** and **[C23](<https://en.wikipedia.org/wiki/C23_(C_standard_revision)>)** syntax
   3. Complete new language features:
      - Struct inheritance/composition
      - defer auto
      - Lambdas
      - (?) Namespaces
      - (?) HolyC ["sub_switch"](<https://harrison.totty.dev/p/a-lang-design-analysis-of-holyc#switch-statements>)
      - (?) Switch `goto` in-switch labels.
   4. Better error messages, help, and man pages.
   5. Multiplatform support *(write similar code in Linux, Windows and MacOS, etc)*
      - Allow the same *(safe)* **SuperC** code to compile and run across platforms without much modification.
      - The goal is to reduce the heavy regions guarded by `#ifdef` macros when working with cross-platform code *(for Windows mostly)*.
- **Standard library**
  - `#include <superc.h>`
  - Strings
  - Arrays
  - [Try/catch](<https://github.com/bruneo32/trycatch>)
  - Date
  - ...
- **Multithreading**
  - Similar to [goroutines](<https://go.dev/ref/mem#go>), **SuperC** must support lightweight threads for massive concurrency, easy to use and manage resources.
  - This implies new syntax.
  - And library functions.
- **Tooling & Editor Extensions**
  1. A language cannot survive today without first-class tooling. Developing a highly responsive **VSCode** extension powered by a robust Language Server Protocol *(LSP)* is a top priority.
  2. Other editors are welcome as well *(Neovim, Emacs, JetBrains, ...)*.

## Documentation
- https://bruneo32.github.io/superc/

Start with **Getting Started**, and explore features with examples.

# Contributing
SuperC is in an early stage - contributions are highly appreciated.

You can help with:
- Compiler development
- Design discussions
- Documentation
- Examples and testing

> - Check issues: https://github.com/bruneo32/superc/issues
> - Read contributing guide: [CONTRIBUTING.md](CONTRIBUTING.md)

# Notes
> Forked from [chibicc](https://github.com/rui314/chibicc).
