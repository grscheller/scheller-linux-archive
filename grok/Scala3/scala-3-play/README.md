# Scala 3 Log

## 2021-02-25: Script kiddie

Start out by

```
   sbt new lampepfl/dotty.g8
```

After a dialog where I overrode default name of the
project, it created 3 directories,

* project
* target
* scala-3-geoffrey-play

and installed locally SBT 1.4.7 (Arch is up to 1.4.6).

```
   $ cat project/build.properties
   sbt.version=1.4.6

   $ cat scala-3-geoffrey-play/project/build.properties
   sbt.version=1.4.7
```

Reasonably sure the top level `project/` and `target/`
directories are artifacts of SBT 1.4.6 and can be ignored.

Now,

```
   $ cd scala-3-geoffrey-play/
   $ sbt console
```

shows me that I am indeed using Scala 3.

From a minimal README.md file which was created and
contained:

### sbt project compiled with Scala 3

#### Usage

This is a normal sbt project. You can compile code with `sbt compile`,
run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project][1].

## 2021-02-25: Setup by Hand

From the above [dotty-example-project][1] link,  I decided to set
everything up by hand.

---

[1]: https://github.com/lampepfl/dotty-example-project/blob/master/README.md
