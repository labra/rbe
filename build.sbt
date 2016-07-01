import sbt._
import sbt.Keys._

lazy val rbe = project.in(file("."))

organization := "es.weso"

name := "rbe"

version := "0.0.9"

scalaVersion := "2.11.8"

publishMavenStyle := true

libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.0.0-M15" 
, "org.typelevel" %% "cats" % "0.6.0-M1"
, "es.weso" % "weso_utils_2.11" % "0.0.15" 
, "es.weso" % "validating_2.11" % "0.0.6" 
)

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// EclipseKeys.useProjectId := true

// Publish site info
site.settings

site.publishSite

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:labra/validating.git"

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/labra/rbe")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/labra/rbe"), "scm:git:git@github.com:labra/validating.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://labra.github.io/rbe/latest/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>labra</id>
        <name>Jose Emilio Labra</name>
        <url>https://github.com/labra/</url>
      </developer>
    </developers>
  ),
  scalacOptions in (Compile,doc) ++= Seq(
    "-Xfatal-warnings",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  )
)
