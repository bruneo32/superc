---
title: Getting started
layout: blog
---

# Getting started with SuperC
Hi everyone! Welcome to **SuperC** documentation.

> ⚠️ **Status:** The compiler is still **experimental**, and the language is **not finalized yet**.
> Expect bugs, missing features, and breaking changes.

## Who are you?
If you landed here, you probably are one of the following...

| You are... | What to do with **SuperC**? |
| ---------- | --------------------------- |
| Curious, just casually looking<br>for new programming languages | Go ahead and try it! Just keep in mind that **SuperC** is still in **beta**<br>Help us focusing on what it *could become*, not what it is today. |
| A *low-level* enjoyer, but **C** is already 50+ years old,<br>and the new low-level languages are too weird or too complex.<br><br>So, you want to take a look at **SuperC**,<br>hoping that this is the one you have been searching for. | Well, right now **SuperC** is unripe, but since you are a **C** guru,<br>we need your help **improving** the *compiler*, *documentation*, *library*, etc.<br><br>**Just** take your time, and **read** some [issues](<https://github.com/bruneo32/superc/issues>){:target="_blank"} or **create** new ones!<br>I do *personally* **thank you**.<br><br>> Thanks to you, this project can become a reality. |

# What is SuperC
> **SuperC** is a **C** superset language, fully backwards compatible with **C**, but with **modern** language ergonomics.

Overall, **SuperC** is a compelling effort to bring the old, reliable **C** to the **modern** world, inspired by languages like *Go*, *Rust* and *Zig* among others.

1. *1972* - **[C](<https://en.wikipedia.org/wiki/C_(programming_language)>){:target="_blank"}** gives you a pistol, very useful, but makes it easy to shoot yourself in the foot.
2. *1997* - **[C++](<https://en.wikipedia.org/wiki/C++>){:target="_blank"}** makes it harder to shoot yourself in the foot, but when you do it blows your whole leg off.
3. *2026* - **SuperC** gives you a pistol with silencer, laser pointer, and better magazine. Why would you shoot yourself in the foot? Just learn to use a pistol!

### Vision
- SuperC attempts to be a **C** successor, improving modern features and zero-cost overhead.
- Slightly unify **C** under the same sun.
  - Source code for the same program can produce errors in different compilers even for the same platform, C actually refers to C dialects depending on which compiler, platform and architecture you are targeting.
  - Since **SuperC** is going to be multiplatform and multiarch, no need for another compiler should arise, then keeping dialects of **SuperC** out of scope.
  - The goal is to collect all the best features from all the C dialects into **SuperC** and document the most possible edge cases and undefined behaviour, for if another **SuperC** compiler is ever created, prevent it from being a dialect.
- Reduce the learning curve of **C** to a minimum for everyone.
  - Right now, most people blindly reject **C** not because it's complicated, but because it's usually poorly taught.
  - The goal is to enhance this documentation to make **SuperC** easy to learn as a beginner, learning **C** in the process.
- Easy integration with existing technology, since **SuperC** is backwards compatible with **C**, there is no need of creating wrappers for existing libraries, for example *[SDL2](<https://wiki.libsdl.org/SDL2/LanguageBindings>){:target="_blank"}*.
  - You can choose to create a wrapper to easily use a library in **SuperC**, but also you can choose not to.

## Roadmap
- **Documentation**
  - Keep [documentation](</>) up-to-date.
  - Provide dual-track explanations: by-the-hand guides for beginners, and straight forward documentation for experienced developers.
  - Use examples *(at least the following)*: simple usage, real usage, and edge cases.
- **Compiler**
   1. Complete **LLVM** backend
      - Right now, the backend is **experimental** x86_64 specific, unoptimized assembly. Just meant to showcase the new language features.
   2. Complete **[C11](<https://en.wikipedia.org/wiki/C11_(C_standard_revision)>){:target="_blank"}** and **[C23](<https://en.wikipedia.org/wiki/C23_(C_standard_revision)>){:target="_blank"}** syntax
   3. Complete new language features:
      - Struct inheritance/composition
      - defer auto
      - Lambdas
      - (?) Namespaces
      - (?) HolyC ["sub_switch"](<https://harrison.totty.dev/p/a-lang-design-analysis-of-holyc#switch-statements>){:target="_blank"}
      - (?) Switch `goto default`.
   4. Better error messages, help, and man pages.
   5. Multiplatform support *(write similar code in Linux, Windows and MacOS, etc)*
      - Allow the same *(safe)* **SuperC** code to compile and run across platforms without much modification.
      - The goal is to reduce the heavy regions guarded by `#ifdef` macros when working with cross-platform code *(for Windows mostly)*.
- **Standard library**
  - `#include <superc.h>`
  - Strings
  - Arrays
  - [Try/catch](<https://github.com/bruneo32/trycatch>){:target="_blank"}
  - Date
  - ...
- **Multithreading**
  - Similar to [goroutines](<https://go.dev/ref/mem#go>){:target="_blank"}, **SuperC** must support lightweight threads for massive concurrency, easy to use and manage resources.
  - This implies new syntax.
  - And library functions.
- **Tooling & Editor Extensions**
  1. A language cannot survive today without first-class tooling. Developing a highly responsive **VSCode** extension powered by a robust Language Server Protocol *(LSP)* is a top priority.
  2. Other editors are welcome as well *(Neovim, Emacs, JetBrains, ...)*.

## Source code
You are welcome to [contribute](<https://github.com/bruneo32/superc/blob/main/CONTRIBUTING.md>){:target="_blank"}.

[https://github.com/bruneo32/superc](<https://github.com/bruneo32/superc>){:target="_blank"}
