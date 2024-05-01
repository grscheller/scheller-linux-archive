# Keywords given and extension in Scala 3

The keywords `given` and `extension` replace the overloaded
keyword `implicit` from Scala 2.  The implicit keyword still
works as it did in Scala 2, but is deprecated and at some
point will be removed.

This project implements an executable
[ScalaImplicits.scala](./src/main/scala/ScalaImplicits.scala)
which is a rewrite of the Scala 2 program
[Scala2Implicits.scala](src/main/scala/Scala2Implicits.scala).

Currently both are being built with Scala3.

This project also implements simple junit tests.
