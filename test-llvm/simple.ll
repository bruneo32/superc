@.str = private unnamed_addr constant [15 x i8] c"Hello, world!\0A\00", align 1

define dso_local i32 @main() #0 {
  %1 = getelementptr inbounds [15 x i8], [15 x i8]* @.str, i64 0, i64 0
  %2 = call i32 (i8*, ...) @printf(i8* noundef %1)
  call void asm sideeffect "incl %eax", "~{rax}"()
  ret i32 42
}

declare i32 @printf(i8* noundef, ...) #1
