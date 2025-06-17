usage:

create output ll and obj

```
zig build
./zig-out/bin/main
```

create print obj

```
zig build-obj src/print.zig -lc --name print
```

using clang as a linker

```
clang output.obj print.obj -o test.exe
```
