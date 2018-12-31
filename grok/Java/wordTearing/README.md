# Java Word Tearing
A java class to test if multiple threads of the underlying
JVM can update adjacent elements of a byte array.

### Background:
* Ideally, separately executing threads, not interacting with
  each other's values, should not need synchronization to ensure
  sequential consistency.  Unfornuately, if the underlying processor
  can only interact on a word basis, synchronization will be needed.
* Most of this code is lifted verbatum from the Java language
  Specification, Java SE 8 Edition.
* Eight Threads working indepently on each byte of a word.
* The threads should not get in each others way unless the
  underlying java implementation can only work on whole words at a time.

### Usage:
* Build via: `javac WordTearing.java`
* Run via:   `java WordTearing`
