# Learn the Zig Programming Language

* Both an interpreter and compiler
* Cleaned up C syntax
* Toolchain very similar to go

1. [Hello World](#hello-world)

## Hello World

Traditional "Hello World" program written in Zig
[hw.zig](hw/hw.zig).

Run as an interpreter:

```bash
   $ zig run hw.zig
   Hello, World!!!
```

Compile and then run:

```bash
   $ zig build-exe hw.zig -O ReleaseSafe
   $ ./hw
   Hello, World!!!
   $ ls -lh hw
   -rwxrwxr-x 1 grs grs 2.1M Feb  5 15:23 hw
```
