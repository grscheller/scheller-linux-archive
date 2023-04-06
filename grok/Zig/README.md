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
   $ zig build-exe hw.zig -O ReleaseSmall
   $ ./hw
   Hello, World!!!
   $ ls -lh hw
   -rwxr-xr-x 1 grs grs 4.5K Nov  2 20:49 hw*
```
