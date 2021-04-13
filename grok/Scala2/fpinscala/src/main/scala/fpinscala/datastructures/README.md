# Functional data structures

Chapter 3.

Implementing List and Binary Tree data structures while working
through the exercises in "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason.

## Trait List in package [fpinscala.datastructures.List](List.scala#L3-L6)

* strict Lisp-like Cons/Nil datastructure

## Program [ListTest](exerciseCode/ListTest.scala)

* program to exercise fpinscala.datastructures.List

## Trait Tree in package [fpinscala.datastructures.Tree](Tree.scala#L3-L6)

* strict tree datastructure
* data is stored in the leaves

## Program [TreeTest](exerciseCode/TreeTest.scala)

* program to exercise fpinscala.datastructures.Tree

Using Scala traits, companion objects, and case classes to implement
datastructures.  Introducing a Scala feature called Abstract Data
Types (ADT) and the distinction between type and data constructors.

In the chapter, the functions are defined as methods of the
companion object.  Basically the List and Tree companion objects
are used as namespaces.
