# Abstract Data Types (ADT) - with Scala

Exploring approaches to ADT in various computer languages.
I was inspired by these blog posts by Tony Morris,
[The Algebra of Algebraic Data Types](https://about.chatroulette.com/posts/algebraic-data-types/)
and
[More Algebra of Algebraic Data Types](https://about.chatroulette.com/posts/algebraic-data-types-2/).

## Scalac directly

To compile, run and cleanup from commandline

```
   $ scalac ADT.scala
   $ scala grok.adt.ADT
   $ rm -r grok/
```

## SBT

Have to use Scala Build Tool (SBT) right now since Scala 3 has
not been added to Arch Repos yet.

```
   $ sbt
   sbt:ADT> compile
   sbt:ADT> run
```
