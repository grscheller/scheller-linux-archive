# Java Word Tearing

## Purpose:
A java class to test if multiple threads of the underlying<br>
JVM can update adjacent elements of a byte array.

## Background:
* Ideally, separately executing threads, not interacting with<br>
  each other's values, should not need synchronization to ensure<br>
  sequential consistency.  Unfornuately, if the underlying processor<br>
  can only interact on a word basis, synchronization will be needed.

* Most of this code is lifted verbatum from the Java language<br>
  Specification, Java SE 8 Edition.

* Eight Threads working indepently on each byte of a word.

* The threads should not get in each others way unless the<br>
  underlying java implementation can only work on whole words at a time.

## Usage:
Build via: `javac WordTearing.java`<br>
Run via:   `java WordTearing`
