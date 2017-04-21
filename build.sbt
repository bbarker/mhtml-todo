enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

name := "mhtml-todo"

scalaVersion := "2.12.1" // or any other Scala version >= 2.10.2
lazy val mhtmlV = "0.3.1"

//libraryDependencies += "in.nvilla" %%% "monadic-rx-cats" % mhtmlV
libraryDependencies += "in.nvilla" %%% "monadic-html" % mhtmlV
libraryDependencies += "in.nvilla" %%% "monadic-rx-cats" % mhtmlV
libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.4"

requiresDOM in Test := true
webpackConfigFile in fastOptJS := Some(baseDirectory.value / "dev.webpack.config.js")
npmDependencies in Compile += "todomvc-app-css" -> "2.1.0"
