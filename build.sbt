name := "game-of-life-comonad"

scalaVersion := "2.12.3"
scalacOptions += "-Ypartial-unification"

mainClass in (Compile, run) := Some("LifeZipper")

libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.0.0-M1",
        "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
