import TwoTails._
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._

lazy val root = (project in file(".")).settings(
  scalaVersion := "2.11.8",
  publishArtifact := false
)
.aggregate(tails)

lazy val tails = build("twotails", "core").settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  )
)