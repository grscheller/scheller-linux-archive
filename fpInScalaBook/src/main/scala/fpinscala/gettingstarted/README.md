# Functional Programming in Scala Book Exercises

## Getting started with functional programming in Scala

Chapter 2.

Learning functional concepts like higher order functions,
tail recursion, polymorphic functions, scala singleton
objects, currying and uncurrying while working through
the exercises in "Functional Programming in Scala" by
Paul Chiusana and Runar Bjarnason.

### fpinscala.test.chap02.gettingstarted.{MyModule,MyModuleRefactored}
   * Singleton objects defined in the file myModules.scala.

### fpinscala.test.chap02.gettingstarted.HigherOrder
   * Explores using functions as first class ojects.

### fpinscala.test.chap02.gettingstarted.MyPolymorphicModule
   * Explores the FP concept of polymorphism.

As a starting point, the book is using standalone objects
as namespaces.  This allows for a more classical introduction
to FP concepts.  "Module" is not a scala term, it is my 
attempt to convey using a singleton object as a namespace.

The use of "chap02" in the qualified package name is being
used to group SBT 'run' and 'test:run' tasks together
by chapter number.  Unlike Java, Scala does not require
the directory structure to mirror the package structure.
