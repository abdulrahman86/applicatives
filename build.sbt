

lazy val commonSettings = Seq(
  organization := "asattar.scala",
  version := "0.1-SNAPSHOT",
  name := "applicative",
  scalaVersion := "2.11.7"
)

lazy val monadGit = ProjectRef(uri("git://github.com/abdulrahman86/monad.git#master"), "monad")
lazy val monoidGit = ProjectRef(uri("git://github.com/abdulrahman86/monoid.git#master"), "monoid")
lazy val streamGit = ProjectRef(uri("git://github.com/abdulrahman86/stream.git#master"), "stream")


lazy val root =
  (project in file("."))
    .settings(commonSettings: _*)
    .dependsOn(monadGit, monoidGit, streamGit)
