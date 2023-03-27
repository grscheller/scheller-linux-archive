# Li Haoyi's os-lib project

Testing out a high level library for filesystem operations.
Also, experimenting with mutilevel Scala builds.

Based on this
[MungingData](https://mungingdata.com/scala/filesystem-paths-move-copy-list-delete-folders/)
blog post.

## Getting started

Bootstrapping from a canned Scala "hello world" project.

```bash
   $ sbt new scala/hello-world.g8
   ...
```

Seems to half created a multiproject build.  Using the above blog post,
a previous build.sbt from my grok/Scala/learnScala, and this
[page](https://www.scala-sbt.org/1.x/docs/sbt-by-example.html)
from the SBT reference manual, I cobbled together a minimal working
example of the library.
