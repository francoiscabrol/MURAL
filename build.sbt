// Project name
name := "mural"

// Project version
version := "0.1.0"

// Scala version
scalaVersion := "2.10.4"

// Java 7: Add dependency on ScalaFX library from Maven repository (Needed by Melvi)
//libraryDependencies += "org.scalafx" %% "scalafx" % "1.0.0-R8"
// Add dependency on JavaFX library based on JAVA_HOME variable (Needed by Melvi)
// unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

// Add dependency with the scala actor library
libraryDependencies <++= scalaVersion(v =>
  Seq("org.scala-lang" % "scala-actors" % v)
)
