# Scala3 Console

Minimalistic SBT config to run scala console

## Preliminaries

Verify Java and SBT versions

```
   $ javac --version
   javac 11.0.11
   $ sbt  --version
   sbt version in this project: 1.5.0
   sbt script version: 1.5.0
```

Idiotically the second command created project/ and target/
directories.  Deleted these.

## Download boilerplate "hello world" project

Follow directions from Scala download
[page](https://docs.scala-lang.org/scala3/getting-started.html)

```
   $ mkdir Scala3
   $ cd scala3
   $ sbt new scala/scala3.g8

   A template to demonstrate a minimal Scala 3 application

   name [Scala 3 Project Template]: scala-console

   Template applied in /home/grs/devel/scheller-linux-archive/grok/Scala3/./scala-console
```

This downloaded a "hello world" template from GitHub.  Lets see what we got

```
   $ find . -type f
   ./scala-console/src/test/scala/Test1.scala
   ./scala-console/src/main/scala/Main.scala
   ./scala-console/build.sbt
   ./scala-console/project/build.properties
   ./scala-console/README.md
   ./README.md
```

The last README.md is this file, the one in `./scala-console`, is
a pretty useless file it pulled down from GitHub.  At least it gives the
[GitHub Project](https://github.com/scala/scala3-example-project/)
location.  Replacing that file with this one and getting rid of some
unnecessary clutter,

```
   mv README.md scala-console/
   rm -rf project/ target/
   cd scala-console
```

## Delete/edit out what is not needed

First, remove source code.

```
   rm -rf src/
```

Next, exit down build.sbt

```
   val scala3Version = "3.0.0"

   lazy val root = project
     .in(file("."))
     .settings(
       name := "scala-console",
       version := "0.1.0",
       scalaVersion := scala3Version
     )
```

## See if it works

First thing `sbt` command did was to locally upgrade itself to 1.5.2.
This was not configured in build.sbt.  Once everything installed, I
relaunched `sbt`.

```
   $ sbt console
   [info] welcome to sbt 1.5.2 (Oracle Corporation Java 11.0.11)
   [info] loading project definition from /home/grs/devel/scheller-linux-archive/grok/Scala3/scala-console/project
   [info] loading settings for project root from build.sbt ...
   [info] set current project to scala-console (in build file:/home/grs/devel/scheller-linux-archive/grok/Scala3/scala-console/)

   scala> val foo = 42
   val foo: Int = 42

   scala> println(s"The secret is ${foo}.")
   The secret is 42.

   scala> foo match
        |   case ii: Int if ii < 42 => println(s"${ii} < 42")
        |   case jj: Int if jj > 42 => println(s"${jj} > 42")
        |   case kk: Int if kk == 42 => println(s"${kk} = 42")
        |
   42 = 42

   scala> scala.util.Properties.versionString
   val res1: String = version 2.13.5

   scala> ^d

   [success] Total time: 18 s, completed May 31, 2021, 3:57:07 AM

   $
```

Running scala outside this project in an empty directory, I get
Arch's Scala version

```
   scala> scala.util.Properties.versionString
   val res1: String = version 2.13.5-20210527-235018-unknown
```

and a `match` without curly brackets is a syntax error.

The reason for the wrong version string is that Scala 3.0.0 reuses
the 2.13.15 standard libraries where this string is defined.

In Scala 3.0.0 `dotty.tools.dotc.config.Properties.versionString`
will return "version 3.0.0".

Sadly, we still have to deal with opaque SBT magic.  I never told SBT
to update itself.