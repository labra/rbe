import sbt._
import sbt.Keys._


lazy val propTest = config("prop") extend (Test)

lazy val rbe = project.in(file(".")).
  configs(propTest).
  settings(publishSettings:_*).
  settings(inConfig(propTest)(Defaults.testSettings): _*)
  

organization := "es.weso"

name := "rbe"

version := "0.0.10"

scalaVersion := "2.11.8"

// crossScalaVersions := Seq("2.11.8", "2.12.0-M5")

publishMavenStyle := true

libraryDependencies ++= Seq(
  compilerPlugin("org.spire-math" %% "kind-projector"   % "0.8.0")
, compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
, compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
, "org.scalatest" %%% "scalatest" % "2.2.6" % "test"
, "org.scalactic" %% "scalactic" % "2.2.6"  
, "org.typelevel" %% "cats" % "0.7.0"
, "es.weso" % "weso_utils_2.11" % "0.0.15" 
, "es.weso" % "validating_2.11" % "0.0.19" 
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

git.remoteRepo := "git@github.com:labra/rbe.git"

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
      "-language:existentials"
    , "-language:higherKinds"
    , "-language:implicitConversions"
    , "-unchecked"
    , "-Xfatal-warnings"
    , "-Xlint"
    , "-Yno-adapted-args"
    , "-Ywarn-dead-code"
    , "-Ywarn-numeric-widen"
    , "-Ywarn-value-discard"
    , "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala"
    , "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
    , "-diagrams"
  )
)
