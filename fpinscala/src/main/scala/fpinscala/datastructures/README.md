# Functional Programming in Scala Book Exercises

## Functional data structures

Chapter 3.

Implementing List and Binary Tree data structures while working<br>
through the exercises in "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

### Package [fpinscala.datastructures.List](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/datastructures/List.scala#L3-L6)
* A strict Lisp-like Cons/Nil datastructure.

### Program [ListTest](ListTest.scala)
* Program to exercise fpinscala.datastructures.List

### Package [fpinscala.datastructures.Tree](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/datastructures/Tree.scala#L3-L6)
* A strict tree datastructure.
* Data is stored in the leaves.

### Program [TreeTest](TreeTest.scala)
* Program to exercise fpinscala.datastructures.Tree

Using Scala traits, companion objects, and case classes to implement<br>
datastructures.  Introducing a Scala feature called Abstract Data<br>
Types (ADT) and the distinction between type and data constructors.

In the chapter, the functions are defined as methods of the<br>
companion object.  Basically the List and Tree companion objects<br>
are used as namespaces.
