import sbt._
import sbt.Keys._
import xerial.sbt.Sonatype._
import xerial.sbt.Sonatype.SonatypeKeys._

object TwoTails{
  val ScalacVersion = "2.11.8"

  def build(pjName: String, base: String) = Project(
    id = pjName,
    base = file(base),
    settings = sonatypeSettings ++
      Seq(
        scalaVersion := ScalacVersion,
        crossVersion := CrossVersion.full,
        crossScalaVersions := Seq("2.11.6", "2.11.7", "2.11.8", "2.12.0", "2.12.1"),
        name := pjName,
        organization := "com.github.wheaties",
        scalacOptions := Seq(
          "-deprecation",
          "-encoding", "UTF-8",
          "-feature",
          "-unchecked"
        ),
        pomExtra := pom,
        publishTo <<= version { v: String =>
          val nexus = "https://oss.sonatype.org/"
          if (v.trim.endsWith("SNAPSHOT"))
            Some("snapshots" at nexus + "content/repositories/snapshots")
          else
            Some("releases" at nexus + "service/local/staging/deploy/maven2")
        },
        credentials += Credentials("Sonatype Nexus Repository Manager",
                           "oss.sonatype.org",
                           "???",
                           "???"),
        pomIncludeRepository := { x => false },
        publishMavenStyle := true,
        publishArtifact in Test := false,
        resolvers ++= Seq(
          Resolver.sonatypeRepo("releases"),
          Resolver.sonatypeRepo("snapshots")
        )
      )
  )

  val pom =
    <url>http://github.com/wheaties/TwoTails</url>
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:wheaties/TwoTails.git</url>
        <connection>scm:git:git@github.com:wheaties/TwoTails.git</connection>
      </scm>
      <developers>
        <developer>
          <id>wheaties</id>
          <name>Owein Reese</name>
          <url>www.github.com/wheaties</url>
        </developer>
      </developers>
}

