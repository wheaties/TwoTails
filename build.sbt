import TwoTails._
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._

lazy val root = (project in file(".")).settings(
  scalaVersion := ScalacVersion,
  crossVersion := CrossVersion.full,
  crossScalaVersions := Seq("2.11.6", "2.11.7", "2.11.8", "2.12.0", "2.12.1"),
  publishArtifact := false
)
.aggregate(plugin, lib)

lazy val plugin = build("twotails", "core").settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ),
  scalacOptions in Test ++= Seq(
    "-Xplugin:" + (packageBin in Compile).value,
    "-verbose",
    "-Xprint-types",
    //"-Xprint-pos",
    //"-Xshow-phases",
    "-Xlog-reflective-calls",
    //"-Xprint:superaccessors",
    //"-Xprint:extmethods",
    //"-Xprint:refchecks",
    "-Xprint:twotails",
    "-Ylog:twotails",
    "-Ycheck:twotails"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  )
)
.dependsOn(lib)

lazy val lib = build("twotails-annotations", "annotations")