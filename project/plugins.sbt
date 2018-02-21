resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.github.os72" % "protoc-jar" % "3.5.0"

// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.10")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.2")

// web plugins

addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.2")

//addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.1")

//addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.1")

//addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.0.0")

//addSbtPlugin("com.typesafe.sbt" % "sbt-mocha" % "1.0.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.22")

addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.6")

addSbtPlugin("org.portable-scala" % "sbt-crossproject"         % "0.3.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.3.1")

val scalaPbVersion = "0.7.0"
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin-shaded" % scalaPbVersion
addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.15" exclude ("com.thesamet.scalapb", "protoc-bridge_2.12"))