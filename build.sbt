import sbt.Keys.publishArtifact
import sbt.Project.projectToRef

lazy val clients = Seq(scalajsclient)

lazy val playserver = (project in file("play")).settings(
  scalaJSProjects := clients,
  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.1.1",
    "org.webjars" %% "webjars-play" % "2.6.3",
    "org.webjars" % "bootstrap" % "3.1.1-2",
    "org.reactivemongo" %% "reactivemongo" % "0.16.2",
    "net.codingwell" %% "scala-guice" % "4.1.1",
    "com.ibm.icu" % "icu4j" % "61.1",
    guice
  ),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  maintainer in Linux := "Arseny Tolmachev <arseny@nlp.ist.i.kyoto-u.ac.jp>",
  name in Linux := "nlp-tools-demo",
  packageName in Linux := (name in Linux).value,
  packageSummary in Linux := "NLP tools demo app",
  packageDescription := "NLP tools demo app",
  serverLoading in Debian := Some(ServerLoader.Systemd)
).enablePlugins(PlayScala, PlayAkkaHttp2Support, DebianPlugin).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(shared2Jvm, `akane-knp-akka`, `akane-jumanpp-grpc`, `grpc-streaming`, `bson-macros`, `akane-testkit` % Test)
  .settings(
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile,doc) := Seq.empty
  )

lazy val scalajsclient = (project in file("scalajs")).settings(
  scalaJSUseMainModuleInitializer := true,
  scalaJSUseMainModuleInitializer in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.4",
    "com.lihaoyi" %%% "scalatags" % "0.6.7",
    "com.github.japgolly.scalajs-react" %%% "core" % "1.2.0",
    "com.github.japgolly.scalajs-react" %%% "extra" % "1.2.0"
  ),
  jsDependencies ++= Seq(
    "org.webjars.npm" % "dagre-d3" % "0.4.17" / "dist/dagre-d3.js" minified "dist/dagre-d3.min.js",
    "org.webjars.npm" % "d3" % "5.1.0" / "dist/d3.js" minified "dist/d3.min.js",
    "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js" minified "2.1.3/jquery.min.js",
    "org.webjars.npm" % "react" % "16.2.0"
      /        "umd/react.development.js"
      minified "umd/react.production.min.js"
      commonJSName "React",

    "org.webjars.npm" % "react-dom" % "16.2.0"
      /         "umd/react-dom.development.js"
      minified  "umd/react-dom.production.min.js"
      dependsOn "umd/react.development.js"
      commonJSName "ReactDOM",

    "org.webjars.npm" % "react-dom" % "16.2.0"
      /         "umd/react-dom-server.browser.development.js"
      minified  "umd/react-dom-server.browser.production.min.js"
      dependsOn "umd/react-dom.development.js"
      commonJSName "ReactDOMServer"
  ),
  dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2"
).enablePlugins(ScalaJSPlugin, ScalaJSWeb).
  dependsOn(shared2Js)

import sbtcrossproject.CrossType
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val shared2 = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared2"))
  .dependsOn(shared)

lazy val shared2Jvm = shared2.jvm
lazy val shared2Js = shared2.js

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .jsConfigure(_ enablePlugins ScalaJSWeb)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.benhutchison" %%% "prickle" % "1.1.13",
      "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
      "org.reactivemongo" %% "reactivemongo-bson-macros" % "0.16.2" % Provided intransitive(),
      "io.grpc" % "grpc-netty" % "1.10.0",
    ),
    PB.targets in Compile := Seq(
      scalapb.gen(grpc=true, flatPackage=true) -> (sourceManaged in Compile).value
    ),
    Compile / PB.protoSources := Seq(
      thisProject.value.base / "../src/main/protobuf"
    ),
    publishArtifact in packageDoc := false,
  )

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// loads the Play project at sbt startup
// onLoad in Global := (Command.process("project playserver", _: State)) compose (onLoad in Global).value

lazy val akane = project in file("akane")

lazy val `akane-knp-akka` = project in file("akane/knp-akka")

lazy val `akane-testkit` = project in file("akane/testkit")

lazy val `akane-jumanpp-grpc` = project in file("akane/jumanpp-grpc")

lazy val `grpc-streaming` = project in file("grpc-akka-stream")

lazy val `bson-macros` = (project in file("bson-macros"))
    .settings(
      libraryDependencies += "org.reactivemongo" %% "reactivemongo" % "0.16.2",
    ).dependsOn(sharedJvm)

fork in run := true

inThisBuild(Seq(
  scalaVersion := "2.12.4",
  version := "0.1"
))