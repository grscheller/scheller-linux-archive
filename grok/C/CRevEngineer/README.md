# C Reverse Engineering

C coding techniques for "system reverse engineering."

## C Feature Test Macros

* **featureTestMacros.c**: [Test compiler options](ftm.md)

## C Raw Terminal Techniques

Build up to a program that works in raw mode.  More or less
following [this guide][1].

[1]: https://viewsourcecode.org/snaptoken/kilo/02.enteringRawMode.html

* **raw01.c**: [Start in canonical "cooked" mode](raw01.c)
* **raw02.c**: [Quit when 'q' is read from input](raw02.c)
* **raw03.c**: [Turn off terminal echoing](raw03.c)
* **raw04.c**: [Return terminal to "cooked" mode at exit](raw04.c)

## Misc. Techniques

* **endianness.c**: [Determine hardware "endianness"](endianess.c)
* **gnuPosixComplianceTrick.h**: [Defined just for POSIX compliance](gnuPosixComplianceTrick.h)
* **korn.c**: [Obfuscated C Code](korn.c)
* **sizeOfThings.c**: [Size in bytes of C constructs](sizeOfThings.c)
