lazy val root = project
  .in(file("."))
  .settings(
    name := "ptda",
    version := "0.0.0",
    scalaVersion := "3.1.3",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

