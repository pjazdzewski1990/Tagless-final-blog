import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val slick = "com.typesafe.slick" %% "slick" % "3.2.0"
  lazy val h2 = "com.h2database" % "h2" % "1.4.196"
}
