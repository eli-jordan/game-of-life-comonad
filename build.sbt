name := "game-of-life-comonad"

scalaVersion := "2.12.3"
scalacOptions += "-Ypartial-unification"

//mainClass in (Compile, run) := Some("LifeStore")

libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "1.0.1",
        "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
