name := "WikipediaProcessor"

version := "0.1"

scalaVersion := "2.12.8"


// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
// libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"
libraryDependencies += "junit" % "junit" % "4.10" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"