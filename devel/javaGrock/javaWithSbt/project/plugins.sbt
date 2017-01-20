// Plug-in to create one-jar (fat-jar)
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

// Plug-in to collect dependent jars into a lib folder
addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.8.0")

// Plug-in to visualize your project's dependencies
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
