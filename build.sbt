name := "game-of-life-comonad"

scalaVersion := "2.12.3"
scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "1.0.1",
        "org.typelevel" %% "cats-effect" % "0.8",
        "org.scalatest" %% "scalatest" % "3.0.4" % "test",
        "io.monix" %% "monix" % "3.0.0-M3",
        "com.typesafe.play" %% "play-json" % "2.6.8"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
