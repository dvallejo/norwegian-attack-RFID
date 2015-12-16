name := "norwegian-attack-RFID"

version := "0.1.0"

scalaVersion  := "2.11.2"

scalacOptions ++= Seq(
  "-deprecation", 
  "-unchecked", 
  "-Xlint",
  "-Ywarn-unused",
  "-Ywarn-unused-import"
)

libraryDependencies +=
  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4"
