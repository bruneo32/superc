---
title: (DRAFT) Lambda Expressions & Closures
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Lambdas are currently not implemented. Syntax may change

# Lambda Expressions & Closures
Lambdas and closures only differ at assembly level, it's nice if you learn the difference, but you don't have to.

In **SuperC**, you can write both as lambdas without worrying about capture. In [other languages](<https://www.geeksforgeeks.org/cpp/lambda-capture-clause-in-cpp/>){:target="_blank"}, you have to explicitly specify captures.

- A ***lambda*** is an **anonymous function** that can be assigned to a variable.
- A ***closure***, is a lambda that **captures** local variables from its environment.

{% tabs example1 %}

{% tab example1 SuperC %}
```c
#include <stdio.h>

int main() {
  auto add = int (int a, int b) {
    // This function is a lambda,
    // because both a and b are parameters
    // There are no variables from 'main' involved.
    return a + b;
  }

  int x = 10;
  auto add_to_x = void (int y) {
    // 'x' is a local from 'main'

    // This function is a closure,
    // because it captures 'x'.
    x += y;

    // If 'x' was global, it would be a lambda,
    // because it would not need to capture it.
  }

  add_to_x(6);
  printf("Closure: %d\n", x);
  printf("Lambda: %d\n", add(2, 3));

  // Closure: 16
  // Lambda: 5
  return 0;
}
```
{% endtab %}

{% tab example1 clang %}
> Note: Actually, clang compiler does not support **fastcall** for x86-64, this code emits a warning.<br>
> <span style="color:fuchsia;">warning:</span> 'fastcall' calling convention is not supported for this target  [-Wignored-attributes]

```c
#include <stdio.h>

__attribute__((fastcall))
static int __lambda0__(int a, int b) {
  return a + b;
}

/**
 * There's no native way to make a closure in C
 * Most language uses a struct to pass the
 * captured environment to the function
 */
struct __closure_env0_t {
  int *x;
};

__attribute__((fastcall))
static void __lambda1__(struct __closure_env0_t __closure_env, int y) {
  // There's not
  *__closure_env.x += y;
}

int main() {
  int x = 10;

  int (*add)(int, int) = __lambda0__;
  void (*add_to_x)(struct __closure_env0_t, int) = __lambda1__;

  struct __closure_env0_t __closure_env0 = { &x };
  add_to_x(__closure_env0, 6);

  printf("Closure: %d\n", x);
  printf("Lambda: %d\n", add(2, 3));

  // Closure: 16
  // Lambda: 5
  return 0;
}
```
{% endtab %}

{% endtabs %}

# Assembly proposal

## Lambdas
Just perform a [fastcall](<https://llvm.org/docs/LangRef.html#calling-conventions>){:target="_blank"}

{% tabs lambda-ass %}

{% tab lambda-ass SuperC %}
```c
#include <stdio.h>

int main() {
  /* no need to assign the lambda to a variable,
   * this way the assembly output is easier to read */

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
; ModuleID = 'lambda.c'
source_filename = "lambda.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

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

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
```
{% endtab %}

{% tab lambda-ass Assembly %}
```sh
llc lambda.ll
```
```s
  .text
  .file	"lambda.c"
# -- Begin function __lambda0__
  .globl	__lambda0__
  .p2align	4, 0x90
  .type	__lambda0__,@function
__lambda0__:  # @__lambda0__
  .cfi_startproc
# %bb.0:
  # kill: def $esi killed $esi def $rsi
  # kill: def $edi killed $edi def $rdi
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
main:  # @main
  .cfi_startproc
# %bb.0:
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

  .type	.L.str,@object  # @.str
  .section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
  .asciz	"OK! %d\n"
  .size	.L.str, 8

  .section	".note.GNU-stack","",@progbits
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
Since *fastcall* does not create a new stack frame, technically the closure has access to the local variables from the enclosing function. So the capture has already happened.
> The key is to precalculate the address of the local variables relative to the stack pointer, in a reliable way.

As you can see in the [assembly output](<?active_tab=closure-ass-assembly#closure-ass>), *__closure0__* does essentially `addl	%esi, 28(%rdi)`, where `%esi` is the parameter **y**, and `28(%rdi)` is the local variable *x*. Effectively performing `x += y`.

{% tabs closure-ass %}

{% tab closure-ass SuperC %}
```c
#include <stdio.h>

int main() {
  int x = 10;

  auto fn = void (int y) {
    x += y;
    // Automatic capture of 'x'
    // -> produce closure instead of lambda
    // in assembly output
  }

  fn(6);
  printf("OK! %d\n", x);

  return 0;
}
```
{% endtab %}

{% tab closure-ass LLVM IR %}
```llvm
; ModuleID = 'xd.c'
source_filename = "xd.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"
@.str = private unnamed_addr constant [8 x i8] c"OK! %d\0A\00", align 1

define dso_local fastcc void @__closure0__(i8* %0, i32 %1) {
  ; compute x location relative to SP
  %3 = ptrtoint i8* %0 to i64
  ; SP is in -32 and x is in -4, so to access x, we need to add (32-4) to SP
  ; This calculus is done by the compiler
  %4 = add i64 %3, 28
  %5 = inttoptr i64 %4 to i32*
  ; x += y
  %6 = load i32, i32* %5
  %7 = add i32 %6, %1
  store i32 %7, i32* %5
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca void (i8*, i32)*, align 8
  store i32 0, i32* %1, align 4
  store i32 10, i32* %2, align 4
  store void (i8*, i32)* @__closure0__, void (i8*, i32)** %3, align 8

  ; save current SP for closure
  %4 = call i8* @llvm.stacksave()
  ; call closure
  %5 = load void (i8*, i32)*, void (i8*, i32)** %3, align 8
  call fastcc void %5(i8* %4, i32 6)

  %6 = load i32, i32* %2, align 4
  %7 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str, i64 0, i64 0), i32 noundef %6)
  ret i32 0
}

declare i32 @printf(i8* noundef, ...) #1
declare i8* @llvm.stacksave()

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
```
{% endtab %}

{% tab closure-ass Assembly %}
```sh
llc closure.ll
```
```s
  .text
  .file	"xd.c"

# -- Begin function __closure0__
  .globl	__closure0__
  .p2align	4, 0x90
  .type	__closure0__,@function
__closure0__:  # @__closure0__
  .cfi_startproc
# %bb.0:
  addl	%esi, 28(%rdi)
  retq
.Lfunc_end0:
  .size	__closure0__, .Lfunc_end0-__closure0__
  .cfi_endproc
# -- End function

# -- Begin function main
  .globl	main
  .p2align	4, 0x90
  .type	main,@function
main:  # @main
  .cfi_startproc
# %bb.0:
  pushq	%rbp
  .cfi_def_cfa_offset 16
  .cfi_offset %rbp, -16
  movq	%rsp, %rbp
  .cfi_def_cfa_register %rbp
  subq	$32, %rsp
  movl	$0, -20(%rbp)
  movl	$10, -4(%rbp)
  movabsq	$__closure0__, %rax
  movq	%rax, -16(%rbp)
  movq	%rsp, %rdi
  movl	$6, %esi
  callq	*-16(%rbp)
  movl	-4(%rbp), %esi
  movabsq	$.L.str, %rdi
  movb	$0, %al
  callq	printf@PLT
  xorl	%eax, %eax
  addq	$32, %rsp
  popq	%rbp
  .cfi_def_cfa %rsp, 8
  retq
.Lfunc_end1:
  .size	main, .Lfunc_end1-main
  .cfi_endproc
# -- End function

  .type	.L.str,@object  # @.str
  .section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
  .asciz	"OK! %d\n"
  .size	.L.str, 8

  .section	".note.GNU-stack","",@progbits
```
{% endtab %}

{% tab closure-ass Output %}
```sh
gcc -no-pie closure.ll
```
```
OK! 16
```
{% endtab %}

{% endtabs %}
