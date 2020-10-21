## SBT Dotty Template

### Usage (from original boilerplate)

This is a normal sbt project, you can compile code
with `sbt compile` and run it with `sbt run`,
`sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).

### How this template arose 
Following these instructions
[Dotty Getting Started](http://dotty.epfl.ch/docs/usage/getting-started.html)
from the EPFL website.
```
   $ mkdir tryDotty; cd tryDotty

   $ sbt --version
   sbt script version: 1.4.0

   $ sbt new lampepfl/dotty.g8
   ...
   A template to demonstrate a minimal Dotty application 
   
   name [Dotty Project Template]: sbtDottyTemplate
   Template applied in /home/grs/devel/scheller-linux-archive/grok/Scala/tryDotty/./dotty-project-template
```
This created
```
   $ ls
   project  sbtdottytemplate  target
```
This seamed to be a SBT 1.4 project which created an SBT 1.3.13
"hello world" project with a single JUnit test..
```
   $ mv sbtdottytemplate/ ../sbtDottyTemplate/
   $ $ cd ../sbtDottyTemplate/
   $ ls
   build.sbt  project  README.md  src
```
I will commit at this point, the only changes so far are
to this README.md file and the name and location of the
directory created.  I have not yet involked SBT.

Commit Tag: 

