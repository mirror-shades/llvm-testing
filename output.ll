; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

@str = private constant [24 x i8] c"Hello from IR compiler!\00"

declare void @print_string(ptr)

declare void @print_int(i32)

define i32 @main() {
entry:
  call void @print_string(ptr @str)
  call void @print_int(i32 2184)
  ret i32 0
}
