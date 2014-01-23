name := "bowling-dojo"

organization := "bowling"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

resolvers += "maven" at "http://central.maven.org/maven2/"

resolvers += "zeebox-public" at "http://nexus.zeebox.com:8080/nexus/content/groups/public"

resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/releases/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.7"

libraryDependencies += "org.hamcrest" % "hamcrest-all" % "1.3"
