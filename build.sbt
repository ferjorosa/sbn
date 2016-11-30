name := "sbn"

val commonSettings = Seq(
	organization := "ferjorosa",
	version := "0.0.1",
	scalaVersion := "2.11.8",
	scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
  resolvers ++= Seq(
    // other resolvers here
    // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  )
)

// ===========  Module 'Core'  =========== //
lazy val core = project.in(file("core"))
  .settings(commonSettings:_*)
  .settings(libraryDependencies ++= Seq (
    // It is ignored
    "org.scala-lang" % "scala-compiler" % "2.11.8",
    // It is ignored
    "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
    "org.scalatest" %% "scalatest" % "3.0.0",
    "ch.qos.logback" %  "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.5.0",
    "org.scala-graph" %% "graph-core" % "1.11.3",
    // https://mvnrepository.com/artifact/org.apache.commons/commons-math3
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "org.scalanlp" %% "breeze" % "0.12")
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
lazy val ltm_server = project.in(file("ltm_server"))
  .dependsOn(ltm)
  .settings(commonSettings:_*)

lazy val main = project.in(file("."))
  .aggregate(core, ltm, ltm_server)