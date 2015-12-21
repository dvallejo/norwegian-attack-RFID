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

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4"

libraryDependencies += "org.scodec" %% "scodec-bits" % "1.0.12"
