# Learn the Gradle build tool

* Tool is JVM based
* Can build both JVM and Native code

1. [Baby Steps](#baby-steps)

## Baby Steps

### first-attempt

Gradle can use Groovy or Kotlin as a DSL in its
build.gradle configuration file.  This example uses Groovy.

```bash
  $ cd first-attempt
  $ gradle -q intro
  Hello World
  I'm Gradle
```

The -q means quiet (log errors only)

```bash
  $ gradle intro

  > Task :hello
  Hello World

  > Task :intro
  I'm Gradle

  BUILD SUCCESSFUL in 445ms
  2 actionable tasks: 2 executed
```

### follow-tutorial

Following a tutorial I found
[here](https://guides.gradle.org/creating-new-gradle-builds/)
on the Gradle website.

  ```bash
    $ mkdir follow-tutorial; cd follow-tutorial
    $ gradle init
    Starting a Gradle Daemon (subsequent builds will be faster)

    Select type of project to generate:
      1: basic
      2: application
      3: library
      4: Gradle plugin
    Enter selection (default: basic) [1..4] 1

    Select build script DSL:
      1: Groovy
      2: Kotlin
    Enter selection (default: Groovy) [1..2] 1

    Project name (default: junk):

    > Task :init
    Get more help with your project: https://guides.gradle.org/creating-new-gradle-builds

    BUILD SUCCESSFUL in 14s
    2 actionable tasks: 2 executed
```

Menu items above seem to have been added post tutorial.
