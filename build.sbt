name := "sbn"

val commonSettings = Seq(
	organization := "ferjorosa",
	version := "0.0.1",
	scalaVersion := "2.11.6",
	scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
)

lazy val core = project.in(file("core"))
  .settings(commonSettings:_*)

lazy val ltm = project.in(file("ltm"))
  .settings(commonSettings:_*)

lazy val ltm_server = project.in(file("ltm-server"))
  .settings(commonSettings:_*)

lazy val main = project.in(file("."))
  .aggregate(core, ltm, ltm_server)
