# Getting started with functional programming in Scala

Chapter 2.

Learning functional concepts like higher order functions, tail
recursion, polymorphic functions, scala singleton objects, currying
and uncurrying while working through the exercises in "Functional
Programming in Scala" by Paul Chiusana and Runar Bjarnason.

## Program [myModule](myModules.scala#L11-L40)

* singleton object defined in the file myModules.scala

## Program [myModuleRefactored](myModules.scala#L42-L79)

* extended version of myModule
* also defined in myModules.scala

## Program [higherOrder](higherOrder.scala#L7-L44)

* explores using functions as first class ojects

## Program [myPolymorphicModule](myPolymorphicModule.scala#L3-L90)

* explores the FP concept of polymorphism

As a starting point, the book is using standalone objects as
namespaces.  This allows for a more classical introduction to FP
concepts.  "Module" is not a scala term, it is my attempt to convey
using singleton objects as a namespaces.  I am not using Scala's
Object Oriented features in any way.

The use of "chap02" in the qualified package names, for the programs
that either test or exercise the libraries, is to group SBT 'run' and
'test:run' tasks together by chapter number.  Unlike Java, Scala doesn't
require the directory structure to mirror the package structure.
