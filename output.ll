; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

@str.1941084635136 = private constant [15 x i8] c"Hello, World!\00\00"

define i32 @main() {
entry:
  %0 = call i32 @puts(ptr @str.1941084635136)
  ret i32 0
}

declare i32 @puts(ptr)
