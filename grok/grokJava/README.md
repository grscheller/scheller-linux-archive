## Java:
An attempt to better familiarize myself with Java coming
at it from a Python and Scala perspective.  Java code
compiles down to byte code which runs on any system
that has a Java Virtual Machine (JVM).

### [Testing JVM for Word Tearing](wordTearing/)
Java program to test for "word tearing" occuring in the
underlying Java Virtual Machine (JVM).  I suspect the
Java 10 compiler is completely "optimizing" the execution
away.

### [Java FX Examples](javafxExamples/)
GUI programming with Java using the FX libraries.  Code currently
does not work with Java 10.  Did work with java 8.

### [Java with Make](javaWithMake/)
Proof of concept to see viability of using GNU make as a Java build
tool.  I dislike Java IDEs due to "the IDE is the language disease."
Also, IDE's tend to be difficult to configure for my visual acuity.
I find it time consumming to get the IDE to the point I can see it.
Once I do, the IDE never works correctly.  I don't think the people
who develop IDEs ever test them with anything other than out of the
box configurations.
