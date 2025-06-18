LLVM bindings taken from https://github.com/kassane/llvm-zig

usage:

create output ll and obj

```
zig build
./zig-out/bin/main
// bootstrapping with zig's drop in C compiler as a linker
zig cc output.obj -o test.exe
```

alternatively you can use clang or preferred as a linker

```
clang output.obj -o test.exe
```
