; ModuleID = 'simple.c'
source_filename = "simple.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Car = type { i8*, %struct.color, i32 }
%struct.color = type { i8, i8, i8, i8 }
%struct.ByteField = type { i8 }
%union.number = type { i32 }
%struct.mypack = type <{ i8, i64 }>
%struct.ShortField = type { i16 }
%struct.ComplexField = type { i16, i16, i32, i8 }
%union.idk_big = type { i64, [504 x i8] }
%struct.inside = type { i32, i32 }

@g001 = dso_local global i8 1, align 1
@g04 = dso_local global double 0.000000e+00, align 8
@g05 = dso_local global x86_fp80 0xK3FFF9E0652141EF0D800, align 16
@g01 = dso_local global i16 0, align 2
@g10 = dso_local global i8* bitcast (i16* @g01 to i8*), align 8
@g2 = internal global i32 5, align 256
@.str = private unnamed_addr constant [7 x i8] c"public\00", align 1
@public_string = dso_local global i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str, i32 0, i32 0), align 8
@.str.1 = private unnamed_addr constant [8 x i8] c"private\00", align 1
@private_string = internal global i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.1, i32 0, i32 0), align 8
@bidimensional = internal global [2 x [3 x i16]] [[3 x i16] [i16 1, i16 2, i16 3], [3 x i16] [i16 4, i16 5, i16 6]], align 2
@bidim_ptr = dso_local global i16* bitcast (i8* getelementptr (i8, i8* bitcast ([2 x [3 x i16]]* @bidimensional to i8*), i64 6) to i16*), align 8
@l = dso_local global i8 108, align 1
@unich1 = dso_local global i16 26085, align 2
@unich2 = dso_local global i32 26412, align 4
@unich3 = dso_local global i32 35486, align 4
@.str.2 = private unnamed_addr constant [10 x i8] c"\E6\97\A5\E6\9C\AC\E8\AA\9E\00", align 1
@unicode1 = dso_local global i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.2, i32 0, i32 0), align 8
@.str.3 = private unnamed_addr constant [4 x i16] [i16 26085, i16 26412, i16 -30050, i16 0], align 2
@unicode2 = dso_local global i16* getelementptr inbounds ([4 x i16], [4 x i16]* @.str.3, i32 0, i32 0), align 8
@.str.4 = private unnamed_addr constant [4 x i32] [i32 26085, i32 26412, i32 35486, i32 0], align 4
@unicode3 = dso_local global i32* getelementptr inbounds ([4 x i32], [4 x i32]* @.str.4, i32 0, i32 0), align 8
@unicode4 = dso_local global i32* getelementptr inbounds ([4 x i32], [4 x i32]* @.str.4, i32 0, i32 0), align 8
@.str.5 = private unnamed_addr constant [4 x i8] c"car\00", align 1
@mycar = dso_local global %struct.Car { i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.5, i32 0, i32 0), %struct.color { i8 -1, i8 0, i8 0, i8 -1 }, i32 3 }, align 8
@bytefield = dso_local global %struct.ByteField { i8 65 }, align 1
@_a = internal global i8 65, align 1
@mynumber = dso_local global %union.number { i32 3 }, align 4
@myfloat = dso_local global { float, [508 x i8] } { float 0x4005AE1480000000, [508 x i8] undef }, align 512
@semistr = dso_local global [4 x i8] c"012\00", align 1
@autoarray = dso_local global [5 x i32] [i32 1, i32 2, i32 3, i32 4, i32 5], align 16
@.str.6 = private unnamed_addr constant [15 x i8] c"anonymous, %c\0A\00", align 1
@g0 = dso_local global i8 0, align 1
@g02 = dso_local global i64 0, align 8
@g03 = dso_local global float 0.000000e+00, align 4
@g06 = dso_local global i128 0, align 16
@g08 = dso_local global i64 0, align 8
@g09 = dso_local global i64 0, align 8
@g1 = dso_local global i32 0, align 512
@mypacked = dso_local global %struct.mypack zeroinitializer, align 1
@shortfield = dso_local global %struct.ShortField zeroinitializer, align 1
@complexfield = dso_local global %struct.ComplexField zeroinitializer, align 4
@mybig = dso_local global %union.idk_big zeroinitializer, align 512
@llvm.compiler.used = appending global [4 x i8*] [i8* bitcast (i32* @g2 to i8*), i8* bitcast (i8** @private_string to i8*), i8* bitcast ([2 x [3 x i16]]* @bidimensional to i8*), i8* @_a], section "llvm.metadata"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.inside, align 1
  %3 = alloca i8*, align 8
  %4 = alloca i64, align 8
  store i32 0, i32* %1, align 4
  %5 = load i8, i8* @g001, align 1
  %6 = trunc i8 %5 to i1
  %7 = zext i1 %6 to i32
  %8 = add nsw i32 48, %7
  %9 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.6, i64 0, i64 0), i32 noundef %8)
  %10 = load i32, i32* @g2, align 256
  %11 = zext i32 %10 to i64
  %12 = call i8* @llvm.stacksave()
  store i8* %12, i8** %3, align 8
  %13 = alloca i32, i64 %11, align 16
  store i64 %11, i64* %4, align 8
  store i32 0, i32* %1, align 4
  %14 = load i8*, i8** %3, align 8
  call void @llvm.stackrestore(i8* %14)
  %15 = load i32, i32* %1, align 4
  ret i32 %15
}

declare i32 @printf(i8* noundef, ...) #1

; Function Attrs: nofree nosync nounwind willreturn
declare i8* @llvm.stacksave() #2

; Function Attrs: nofree nosync nounwind willreturn
declare void @llvm.stackrestore(i8*) #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nofree nosync nounwind willreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Debian clang version 14.0.6"}
