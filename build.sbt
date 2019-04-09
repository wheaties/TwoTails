import TwoTails._

lazy val root = (project in file(".")).settings(
  scalaVersion := ScalacVersion,
  crossVersion := CrossVersion.full,
  crossScalaVersions := Seq("2.11.6", "2.11.7", "2.11.8", "2.12.0", "2.12.1", "2.13.0-RC1"),
  publishArtifact := false
)
.aggregate(plugin, lib)

lazy val plugin = build("twotails", "core").settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ),
  unmanagedSourceDirectories in Compile += {
    val base = baseDirectory.value / "src/main"
    val dir = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        "scala-2.13+"
      case _ =>
        "scala-2.13-"
    }
    base / dir
  },
  scalacOptions in Test ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2L, v)) if v >= 13 =>
        Nil
      case _ =>
        Seq("-Yno-adapted-args")
    }
  },
  scalacOptions in Test ++= Seq(
    "-Xplugin:" + (packageBin in Compile).value,
    "-verbose",
    "-Ywarn-value-discard",
    "-Ywarn-dead-code",
    //"-P:twotails:memory",
    "-Xprint-types",
    "-Xlog-reflective-calls",
    //"-Xprint:twotails",
    "-Ylog:twotails"//,
    //"-Ycheck:twotails"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.8-RC2" % Test
  )
)
.dependsOn(lib)

lazy val lib = build("twotails-annotations", "annotations")
