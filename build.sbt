import sbt.Keys.publishArtifact
import sbt.Project.projectToRef

lazy val clients = Seq(scalajsclient)

lazy val playserver = (project in file("play")).settings(
  scalaJSProjects := clients,
  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.1.1",
    "org.webjars" %% "webjars-play" % "2.6.3",
    "org.webjars" % "bootstrap" % "3.1.1-2",
    "org.reactivemongo" %% "reactivemongo" % "0.12.7",
    "net.codingwell" %% "scala-guice" % "4.1.1",
    guice
  ),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  maintainer in Linux := "Arseny Tolmachev <arseny@nlp.ist.i.kyoto-u.ac.jp>",
  name in Linux := "nlp-tools-demo",
  packageName in Linux := (name in Linux).value,
  packageSummary in Linux := "NLP tools demo app",
  packageDescription := "NLP tools demo app",
  serverLoading in Debian := Some(ServerLoader.Systemd)
).enablePlugins(PlayScala, DebianPlugin).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(sharedJvm, `akane-knp-akka`, `akane-testkit` % Test)
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
    "com.github.japgolly.scalajs-react" %%% "core" % "1.1.1",
    "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.1",
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % "1.1.1"
  ),
  jsDependencies ++= Seq(
    "org.webjars.npm" % "dagre-d3" % "0.4.17" / "dist/dagre-d3.js" minified "dist/dagre-d3.min.js",
    "org.webjars.npm" % "d3" % "3.5.17" / "3.5.17/d3.js" minified "3.5.17/d3.min.js",
    "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js" minified "2.1.3/jquery.min.js",
    "org.webjars.bower" % "react" % "15.3.2"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % "15.3.2"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars.bower" % "react" % "15.3.2"
      /         "react-dom-server.js"
      minified  "react-dom-server.min.js"
      dependsOn "react-dom.js"
      commonJSName "ReactDOMServer"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSWeb).
  dependsOn(sharedJs)

import sbtcrossproject.{crossProject, CrossType}

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
      "org.reactivemongo" %% "reactivemongo" % "0.12.7" % Provided intransitive(),
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

fork in run := true

inThisBuild(
  scalaVersion := "2.12.4"
)