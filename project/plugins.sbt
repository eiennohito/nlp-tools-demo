resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.github.os72" % "protoc-jar" % "3.0.0-b3"

val scalaPbVersion = "0.5.32"

addSbtPlugin("com.trueaccord.scalapb" % "sbt-scalapb" % scalaPbVersion)

// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.8")

// web plugins

addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.0")

//addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.1")

//addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.1")

//addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.0.0")

//addSbtPlugin("com.typesafe.sbt" % "sbt-mocha" % "1.0.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.12")

addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.1")
