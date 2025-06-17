; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

@int_format = private constant [5 x i8] c"%d\0A\00\00"
@str = private constant [24 x i8] c"Hello from IR compiler!\00"

define void @print_string(ptr %0) {
entry:
  %1 = call i32 @puts(ptr %0)
  ret void
}

declare i32 @puts(ptr)

define void @print_int(i32 %0) {
entry:
  %1 = call i32 (ptr, ...) @printf(ptr @int_format, i32 %0)
  ret void
}

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  call void @print_string(ptr @str)
  call void @print_int(i32 2184)
  ret i32 0
}
