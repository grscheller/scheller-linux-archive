# Java
An attempt to familiarize myself with Java coming
at it from a Python and Scala perspective.  Java code
compiles down to byte code which runs on any system that
implements a Java Virtual Machine (JVM).  Byte code is
the "machine code" of the virtual machine.  A useful
optimization of the JVM is Just in Time Complilation.
The JVM converts portions of byte code to actual machine
code of the underlying hardware.
* [Testing JVM for Word Tearing](wordTearing/)
  - Test for "word tearing" in underlying Java Virtual Machine (JVM)
  - I suspect the Java 10 compiler "optimizing" away execution
* [Java FX Examples](javafxExamples/)
  - GUI programming using Java FX libraries
  - Code currently does not work with Java 10
  - Code did work with java 8.
* [Java with Make](javaWithMake/)
  - Proof of concept of GNU make as a Java build tool
