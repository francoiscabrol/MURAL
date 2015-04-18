import sbt.Keys._
import sbt._

object MuralBuild extends Build {

  //////////////////////////////////////////////////////////////////////////////
  // PROJECT INFO
  //////////////////////////////////////////////////////////////////////////////

  val ORGANIZATION = "francoiscabrol"
  val PROJECT_NAME = "mural"
  val PROJECT_VERSION = "0.2.0"
  val SCALA_VERSION = "2.11.4"

  //////////////////////////////////////////////////////////////////////////////
  // PROJECTS
  //////////////////////////////////////////////////////////////////////////////

  lazy val root = Project(
    id = PROJECT_NAME,
    base = file("."),
    settings = muralSettings
  ).dependsOn(libjamu, melvi)

  lazy val melvi = uri("git://github.com/francoiscabrol/MelVi")
  lazy val libjamu = uri("git://github.com/francoiscabrol/libjamu")


  //////////////////////////////////////////////////////////////////////////////
  // SETTINGS
  //////////////////////////////////////////////////////////////////////////////

  lazy val muralSettings = Project.defaultSettings ++ basicSettings

  lazy val basicSettings = Seq(
    version := PROJECT_VERSION,
    organization := ORGANIZATION,
    scalaVersion := SCALA_VERSION,

    libraryDependencies <++= scalaVersion(v =>
      Seq("org.scala-lang" % "scala-actors" % v)
    ),

    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test",

    javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint"),

    initialize := {
      val _ = initialize.value
      if (sys.props("java.specification.version") != "1.8")
        sys.error("Java 8 is required for this project.")
    },

    unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/ext/jfxrt.jar")),

    connectInput in run := true,

    fork in run := true
  )
}
