name := "game-of-life-comonad"

scalaVersion := "2.13.6"

mainClass in (Compile, run) := Some("LifeZipper")

libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.7.0",
        "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
