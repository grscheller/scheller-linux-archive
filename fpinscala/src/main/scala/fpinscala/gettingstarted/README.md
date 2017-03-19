# Functional Programming in Scala Book Exercises

## Getting started with functional programming in Scala

Chapter 2.

Learning functional concepts like higher order functions, tail<br>
recursion, polymorphic functions, scala singleton objects, currying<br>
and uncurrying while working through the exercises in "Functionali<br>
Programming in Scala" by Paul Chiusana and Runar Bjarnason.

### [fpinscala.chap02.gettingstarted.{MyModule,MyModuleRefactored}](myModules.scala)
* Singleton objects defined in the file myModules.scala.

### [fpinscala.chap02.gettingstarted.HigherOrder](HigherOrder.scala)
* Explores using functions as first class ojects.

### [fpinscala.chap02.gettingstarted.MyPolymorphicModule](MyPolymorphicModule.scala)
* Explores the FP concept of polymorphism.

As a starting point, the book is using standalone objects as<br>
namespaces.  This allows for a more classical introduction to FP<br>
concepts.  "Module" is not a scala term, it is my attempt to convey<br>
using a singleton object as a namespace.

The use of "chap02" in the qualified package name is being used to<br>
group SBT 'run' and 'test:run' tasks together by chapter number.<br>
Unlike Java, Scala does not require the directory structure to<br>
mirror the package structure.
