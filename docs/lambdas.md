---
title: (DRAFT) Lambda Expressions & Closures
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Lambdas are currently **not implemented**. Syntax may change<br>
> Development is not going to advance until [LLVM backend](/#roadmap) is minimally stable.

# Lambda Expressions & Closures
> Typically used for short, simple operations. Many functions *(like map, filter, sort, ...)* expect another function as input to tell them what to do, lambdas are the perfect, lightweight way to provide that instruction.

Lambdas and closures only differ at assembly level, it's nice if you learn the difference, but you don't have to.

In **SuperC**, you can write both as lambdas without worrying about capture. In [other languages](<https://www.geeksforgeeks.org/cpp/lambda-capture-clause-in-cpp/>){:target="_blank"}, you have to explicitly specify captures.

- A ***lambda***, is an **anonymous function** that can only be referenced in it's declaration scope, although it lives in the global context.
- A ***closure***, is a lambda that **captures** local variables from its environment.

{% tabs lambdas1 %}

{% tab lambdas1 SuperC %}
```c
#include <stdio.h>

int main() {
  int add1(int a, int b) {
    // This function is a lambda,
    // because both a and b are parameters
    // There are no variables from 'main' involved.
    return a + b;
  }

  // You can assign a lambda or closure to a variable
  auto add2 = int (int c, int d) {
    return c + d;
  };


  int x = 10;
  void add_to_x(int y) {
    // This function is a closure,
    // because it captures 'x' from 'main'
    x += y;

    // If 'x' was global, it would be a lambda,
    // because globals do not need capture
  }

  add_to_x(6);

  printf("Lambda1: %d\n", add1(2, 3));
  printf("Lambda2: %d\n", add2(5, 3));
  printf("Closure: %d\n", x);

  // Lambda1: 5
  // Lambda2: 8
  // Closure: 16
  return 0;
}
```
{% endtab %}

{% tab lambdas1 Pseudo-C %}
```c
#include <stdio.h>

__attribute__((fastcall))
static int __lambda0__(int a, int b) {
  return a + b;
}

__attribute__((fastcall))
static int __lambda1__(int c, int d) {
  return c + d;
}

static _Thread_local void *__main_frame;

__attribute__((fastcall))
static void __lambda2__(int y) {
  /* == Closure prologue (captured variables) == */
  int *x = (int *)(__main_frame - 4);

  /* == Closure body == */
  *x += y;
}

int main() {
  /* There's no native way to do this in C,
   * but let's imagine that we save the
   * the stack pointer before the
   * locals allocation.
   * This way, __main_frame represents where
   * the locals start in memory. */
  __main_frame = __builtin_stack_pointer();


  int x = 10;
  __lambda2__(6);

  int (*add2)(int, int) = __lambda1__;

  printf("Lambda1: %d\n", __lambda0__(2, 3));
  printf("Lambda2: %d\n", add2(5, 3));
  printf("Closure: %d\n", x);

  // Lambda1: 5
  // Lambda2: 8
  // Closure: 16
  return 0;
}
```
{% endtab %}

{% endtabs %}

# Assembly proposal

## Lambdas
- A lambda function is registered as any other function in the global internal scope, but the name and symbol are mangled.
- Lambdas use [fastcall](<https://llvm.org/docs/LangRef.html#calling-conventions>){:target="_blank"} by default. This can be overridden using `__attribute__((stdcall))`.

{% tabs lambda-ass %}

{% tab lambda-ass SuperC %}
```c
#include <stdio.h>

int main() {

  int lambda_function(int a, int b) {
    return a + b;
  }

  int x = lambda_function(2, 3);
  printf("OK! %d\n", x);

  return 0;
}
```
{% endtab %}

{% tab lambda-ass LLVM IR %}
```llvm
...

@.str = private unnamed_addr constant [8 x i8] c"OK! %d\0A\00", align 1

define dso_local fastcc i32 @__lambda0__(i32 noundef %0, i32 noundef %1) {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %4, align 4
  %7 = add nsw i32 %5, %6
  ret i32 %7
}

define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %3 = call fastcc i32 @__lambda0__(i32 noundef 2, i32 noundef 3)
  store i32 %3, i32* %2, align 4
  %4 = load i32, i32* %2, align 4
  %5 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str, i64 0, i64 0), i32 noundef %4)
  ret i32 0
}

declare i32 @printf(i8* noundef, ...) #1

...
```
{% endtab %}

{% tab lambda-ass Assembly %}
```sh
llc lambda.ll
```
```s
...

# -- Begin function __lambda0__
  .globl	__lambda0__
  .p2align	4, 0x90
  .type	__lambda0__,@function

__lambda0__:
  .cfi_startproc
  movl	%edi, -4(%rsp)
  movl	%esi, -8(%rsp)
  leal	(%rdi,%rsi), %eax
  retq
.Lfunc_end0:
  .size	__lambda0__, .Lfunc_end0-__lambda0__
  .cfi_endproc
# -- End function


# -- Begin function main
  .globl	main
  .p2align	4, 0x90
  .type	main,@function

main:
  .cfi_startproc
  pushq	%rbp
  .cfi_def_cfa_offset 16
  .cfi_offset %rbp, -16
  movq	%rsp, %rbp
  .cfi_def_cfa_register %rbp
  subq	$16, %rsp

  movl	$0, -8(%rbp)
  movl	$2, %edi
  movl	$3, %esi
  callq	__lambda0__
  movl	%eax, -4(%rbp)
  movl	-4(%rbp), %esi
  movabsq	$.L.str, %rdi
  movb	$0, %al
  callq	printf@PLT
  xorl	%eax, %eax

  addq	$16, %rsp
  popq	%rbp
  .cfi_def_cfa %rsp, 8
  retq
.Lfunc_end1:
  .size	main, .Lfunc_end1-main
  .cfi_endproc
# -- End function


# @.str
  .type	.L.str,@object
  .section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
  .asciz	"OK! %d\n"
  .size	.L.str, 8

...
```
{% endtab %}

{% tab lambda-ass Output %}
```sh
gcc -no-pie lambda.s
```
```
OK! 5
```
{% endtab %}

{% endtabs %}

## Closures
In order for a *closure* to access a variable from its parent function, the closure **needs** to know **two** things in advance:
1. the ***frame pointer*** of the **parent** function.
2. the ***offset*** of the **local** variable inside the *parent frame*.

This way, we can access the *"captured" variable* by just *adding* or *substracting* the *offset* of a variable to the *parent's frame pointer*.

But there are some caveats to overcome:
- In a multicore CPU, each core can call a function with a different frame pointer, so we need to save the *parent's frame pointer* in a **thread-safe** variable.
- If there's a *middle frame (like `foreach` in the following example)*, then we cannot assume that the *parent frame* is the *caller frame* anymore, so a secret **[thread_local](<https://en.cppreference.com/c/language/storage_duration>){:target="_blank"} global variable** is used to store the *parent's frame pointer*.
- If the parent function is *fastcall* or compiled with `-fomit-frame-pointer`, then the frame pointer is omitted. So we can save the ***stack pointer*** *(instead of the frame pointer)* at the *prologue* of the parent function, effectively saving the *frame pointer*, against all odds.

Regarding [storage specifiers](<https://en.cppreference.com/c/language/storage_duration>){:target="_blank"}:
- The **storage-specifier** of a *captured* variable CANNOT be **register**, you must pass the variable as an argument.
- The **storage-specifier** of a *captured* variable can be **static**, but ***static local variables*** are actually **hidden internal globals**, so a capturing a *static local variable* produces a **lambda** instead of a **closure**.

{% tabs closure-ass %}

{% tab closure-ass SuperC %}
```c
#include <stdio.h>

void foreach(int *list, int count, void (*callback)(int)) {
  for (int i = 0; i < count; i++) {
    callback(list[i]);
  }
}

int main() {
  // Fill list with 100 numbers
  int list[100];
  for (int i = 0; i < 100; i++)
    list[i] = i;

  int b = 11;
  foreach(list, 100, void (int a) {
    // Automatic capture of 'b'
    printf("%d: %d\n", a, b);
  });

  // 0: 11
  // 1: 11
  // ...
  // 98: 11
  // 99: 11
  return 0;
}
```
{% endtab %}

{% tab closure-ass LLVM IR %}
```llvm
...

@__main_frame = dso_local thread_local global i8* null, align 8
@.str = private unnamed_addr constant [8 x i8] c"%d: %d\0A\00", align 1

...

; Omitted '@foreach' since is irrelevant for this example

define dso_local i32 @main() #0 {
  ; Save the current stack pointer.
  ; i.e., save frame pointer in @__main_frame, even if -fno-omit-frame-pointer is used.
  %1 = call i8* @llvm.stacksave()
  store i8* %1, i8** @__main_frame, align 8

  %2 = alloca i32, align 4
  %3 = alloca [100 x i32], align 16
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 0, i32* %2, align 4
  store i32 0, i32* %4, align 4
  br label %6

6:
  %7 = load i32, i32* %4, align 4
  %8 = icmp slt i32 %7, 100
  br i1 %8, label %9, label %17

9:
  %10 = load i32, i32* %4, align 4
  %11 = load i32, i32* %4, align 4
  %12 = sext i32 %11 to i64
  %13 = getelementptr inbounds [100 x i32], [100 x i32]* %3, i64 0, i64 %12
  store i32 %10, i32* %13, align 4
  br label %14

14:
  %15 = load i32, i32* %4, align 4
  %16 = add nsw i32 %15, 1
  store i32 %16, i32* %4, align 4
  br label %6, !llvm.loop !8

17:
  store i32 10, i32* %5, align 4
  %18 = getelementptr inbounds [100 x i32], [100 x i32]* %3, i64 0, i64 0
  call void @foreach(i32* noundef %18, i32 noundef 100, void (i32)* noundef @closure0)
  ret i32 0
}

define dso_local fastcc void @__closure0__(i32 noundef %0) #0 {
  %2 = alloca i32, align 4  ; argument a
  %3 = alloca i32, align 4  ; captured b
  store i32 %0, i32* %2, align 4

  ; closure prologue (captured variables)
  %4 = load i8*, i8** @__main_frame, align 8
  %5 = getelementptr i8, i8* %4, i64 -424
  %6 = bitcast i8* %5 to i32*
  %7 = load i32, i32* %6, align 4
  store i32 %7, i32* %3, align 4 ; captured b = [main_frame - 424]

  ; closure body
  %8 = load i32, i32* %2, align 4
  %9 = load i32, i32* %3, align 4
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str, i64 0, i64 0), i32 noundef %8, i32 noundef %9)
  ret void
}

declare i32 @printf(i8* noundef, ...) #1
declare i8* @llvm.stacksave()

...
```
{% endtab %}

{% tab closure-ass Assembly %}
```sh
llc closure.ll
```
```s
...

# -- Begin function main
  .globl	main
  .p2align	4, 0x90
  .type	main,@function

main:
  .cfi_startproc
  pushq	%rbp
  .cfi_def_cfa_offset 16
  .cfi_offset %rbp, -16
  movq	%rsp, %rbp
  .cfi_def_cfa_register %rbp
  subq	$416, %rsp

  # Save the current stack pointer.
  # i.e., save frame pointer in @__main_frame, even if -fno-omit-frame-pointer is used.
  movq	%rsp, %fs:__main_frame@TPOFF
  movl	$0, -12(%rbp)
  movl	$0, -4(%rbp)

.LBB1_1:
  cmpl	$100, -4(%rbp)
  jge	.LBB1_4

  movl	-4(%rbp), %eax
  movslq	-4(%rbp), %rcx
  movl	%eax, -416(%rbp,%rcx,4)

  movl	-4(%rbp), %eax
  addl	$1, %eax
  movl	%eax, -4(%rbp)
  jmp	.LBB1_1

.LBB1_4:
  movl	$10, -8(%rbp)
  leaq	-416(%rbp), %rdi
  movabsq	$closure0, %rdx
  movl	$100, %esi
  callq	foreach
  xorl	%eax, %eax

  addq	$416, %rsp
  popq	%rbp
  .cfi_def_cfa %rsp, 8
  retq

.Lfunc_end1:
  .size	main, .Lfunc_end1-main
  .cfi_endproc
# -- End function


# -- Begin function closure0
  .globl	closure0
  .p2align	4, 0x90
  .type	closure0,@function

closure0:
  .cfi_startproc
  pushq	%rbp
  .cfi_def_cfa_offset 16
  .cfi_offset %rbp, -16
  movq	%rsp, %rbp
  .cfi_def_cfa_register %rbp
  subq	$16, %rsp

  movl	%edi, -8(%rbp)

  # closure prologue (captured variables)
  movq	%fs:__main_frame@TPOFF, %rax
  movl	-424(%rax), %eax
  movl	%eax, -4(%rbp)

  # closure body
  movl	-8(%rbp), %esi
  movl	-4(%rbp), %edx
  movabsq	$.L.str, %rdi
  movb	$0, %al
  callq	printf@PLT

  addq	$16, %rsp
  popq	%rbp
  .cfi_def_cfa %rsp, 8
  retq

.Lfunc_end2:
  .size	closure0, .Lfunc_end2-closure0
  .cfi_endproc
# -- End function


# @__main_frame
  .type	__main_frame,@object
  .section	.tbss,"awT",@nobits
  .globl	__main_frame
  .p2align	3
__main_frame:
  .quad	0
  .size	__main_frame, 8


# @.str
  .type	.L.str,@object
  .section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
  .asciz	"%d: %d\n"
  .size	.L.str, 8

...
```
{% endtab %}

{% tab closure-ass Output %}
```sh
gcc -no-pie closure.ll
```
```
0: 11
1: 11
...
98: 11
99: 11
```
{% endtab %}

{% endtabs %}
