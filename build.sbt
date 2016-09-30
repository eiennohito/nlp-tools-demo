import sbt.Project.projectToRef

lazy val clients = Seq(scalajsclient)
lazy val scalaV = "2.11.8"

lazy val playserver = (project in file("play")).settings(
  scalaVersion := scalaV,
  scalaJSProjects := clients,
  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.0.0",
    "org.webjars" %% "webjars-play" % "2.5.0",
    "com.lihaoyi" %% "upickle" % "0.4.1",
    "org.reactivemongo" %% "reactivemongo" % "0.11.14"
  ),
  pipelineStages in Assets := Seq(scalaJSPipeline)
).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(sharedJvm, `akane-knp-akka`)

lazy val scalajsclient = (project in file("scalajs")).settings(
  scalaVersion := scalaV,
  persistLauncher := true,
  persistLauncher in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "com.lihaoyi" %%% "upickle" % "0.4.1",
    "org.querki" %%% "jquery-facade" % "1.0-RC6",
    "com.lihaoyi" %%% "scalatags" % "0.6.0"
  ),
  jsDependencies ++= Seq(
    "org.webjars.npm" % "dagre-d3" % "0.4.17" / "dist/dagre-d3.js" minified "dist/dagre-d3.min.js",
    "org.webjars.npm" % "d3" % "3.5.17" / "3.5.17/d3.js" minified "3.5.17/d3.min.js",
    "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js" minified "2.1.3/jquery.min.js"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSWeb).
  dependsOn(sharedJs)

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(scalaVersion := scalaV).
  jsConfigure(_ enablePlugins ScalaJSWeb)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// loads the Play project at sbt startup
onLoad in Global := (Command.process("project playserver", _: State)) compose (onLoad in Global).value

lazy val akane = (project in file("akane"))
lazy val `akane-knp-akka` = (project in file("akane/knp-akka"))


fork in run := true
