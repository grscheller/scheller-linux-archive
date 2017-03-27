## Getting started with functional programming in Scala

Chapter 2.

Learning functional concepts like higher order functions, tail<br>
recursion, polymorphic functions, scala singleton objects, currying<br>
and uncurrying while working through the exercises in "Functional<br>
Programming in Scala" by Paul Chiusana and Runar Bjarnason.

### Program [myModule](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/gettingstarted/myModules.scala#L12-L40)
* Singleton object defined in the file myModules.scala.

### Program [myModuleRefactored](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/gettingstarted/myModules.scala#L45-L74)
* Extended version of myModule.
* Also defined in myModules.scala.

### Program [higherOrder](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/gettingstarted/higherOrder.scala#L7-L44)
* Explores using functions as first class ojects.

### Program [myPolymorphicModule](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/gettingstarted/myPolymorphicModule.scala#L6-L90)
* Explores the FP concept of polymorphism.

As a starting point, the book is using standalone objects as<br>
namespaces.  This allows for a more classical introduction to FP<br>
concepts.  "Module" is not a scala term, it is my attempt to convey<br>
using singleton objects as a namespaces.  I am not using Scala's<br>
Object Oriented features in any way.

The use of "chap02" in the qualified package names, for the programs<br>
that either test or exercise the libraries, is to group SBT 'run' and<br>
'test:run' tasks together by chapter number.  Unlike Java, Scala doesn't<br>
require the directory structure to mirror the package structure.
