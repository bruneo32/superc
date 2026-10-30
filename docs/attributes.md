---
layout: blog
title: (DRAFT) SuperC attributes
---

> ⚠️ This is a **PROPOSAL DRAFT**. Some attributes are currently **not implemented**. Syntax may change<br>

# SuperC attributes
Attributes can be expressed in **SuperC** using either the `__attribute__` keyword or the `[[attribute]]` syntax *(since C23)*

Also, the attributes list can be defined at different places in ***functions***, ***variables*** and ***structs/unions***.

```c
// Before variable declaration
[[attributes]]
int var = 0;

// Before variable name declaration
int [[attributes]] var = 0;

// After variable name declaration
int var [[attributes]] = 0;

// Before function declaration
[[attributes]]
void function(int a) { ... }

// After function declaration
void function(int a) [[attributes]] { ... }

// Before struct/union declaration
[[attributes]]
struct point {
  int a;
  int b;
};

// After struct/union name
struct point [[attributes]] {
  int a;
  int b;
};

// After struct/union declaration
struct point {
  int a;
  int b;
} [[attributes]];
```

# Attributes list

## \_\_attribute\_\_((aligned))
Changes the alignment of the type in memory.

## \_\_attribute\_\_((stdcall))
> Not implemented yet

The default calling convention.

## \_\_attribute\_\_((fastcall))
> Not implemented yet

Uses the [fastcall](<https://llvm.org/docs/LangRef.html#calling-conventions>){:target="_blank"} calling convention, which is faster than the default calling convention.

- This calling convention does not support **varargs**.

## \_\_attribute\_\_((linkonce))
> Not implemented yet

If the same **symbol** is defined in multiple **object files**, the linker will only keep one copy of the symbol, and will discard all but one of the definitions.

## \_\_attribute\_\_((packed))
Makes the *struct/union* members *packed*, so they are aligned to their natural *size*.

## \_\_attribute\_\_((symbol))
Changes the symbol of the *variable* or *function*. See [symbols](symbols.md) for more information.

## \_\_attribute\_\_((noinit))
> Not implemented yet

Prevents aggregate initialization and compound-literal initialization of the attributed type outside its declaration file *(same \_\_FILE\_\_)*.

## \_\_attribute\_\_((closed))
> Not implemented yet

Prevents defining new type methods outside the declaration file of the receiver's declaring type.

A method must be declared in the same declaration file as its receiver's declaring type. Its implementation may reside in another file, provided its declaration belongs to the type's declaration file.

## \_\_attribute\_\_((access))
> Not implemented yet

Controls the **access permissions** of struct **members**, and the call *visibility* of type **methods**.
- With **one** argument, sets both **read** and **write** *permissions*
- With **two** arguments, the first specifies **read** *permission* and the second specifies **write** *permission*

For type methods:
- The **read** argument specifies ***call visibility***
- The **write** argument is *ignored*

Access levels:
- **public**: accessible from any scope.
- **private**: accessible only from methods whose receiver is the declaring type.
- **protected**: accessible from methods whose receiver is the declaring type or one of its descendants.

Access permissions are checked at compile time and do not change the memory representation or ABI of the type.

```c
// Set both read and write permission to private
__attribute__((access(private)))

// Set read permission to public, and write to private
__attribute__((access(public, private)))
```
