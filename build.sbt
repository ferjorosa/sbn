name := "sbn"

val commonSettings = Seq(
	organization := "ferjorosa",
	version := "0.0.1",
	scalaVersion := "2.11.6",
	scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
)

// ===========  Module 'Core'  =========== //
lazy val core = project.in(file("core"))
  .settings(commonSettings:_*)
  .settings(libraryDependencies ++= Seq (
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scalactic" %% "scalactic" % "3.0.0",
    "ch.qos.logback" %  "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.5.0")
  )

// ===========  Module 'Ltm'  ============ //
lazy val ltm = project.in(file("ltm"))
  .dependsOn(core)
  .settings(commonSettings:_*)

// =========  Module 'Examples'  ========= //
lazy val examples = project.in(file("examples"))
  .dependsOn(core,ltm)
  .settings(commonSettings:_*)

// ========  Module 'Ltm-Server'  ======== //
lazy val ltm_server = project.in(file("ltm-server"))
  .dependsOn(ltm)
  .settings(commonSettings:_*)

lazy val main = project.in(file("."))
  .aggregate(core, ltm, ltm_server)
