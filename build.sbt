name := "wadler"

version in ThisBuild := "0.0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.2"

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature", "-language:_")

resolvers in ThisBuild ++= Seq(
  "mvn-local" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
, Classpaths.typesafeResolver
, "atlassian-public" at "https://m2proxy.atlassian.com/content/groups/atlassian-public/"
, "atlassian-internal" at "https://m2proxy.atlassian.com/content/groups/internal/"
)

libraryDependencies ++= Seq(
  "org.scalaz"                       %% "scalaz-core"        % "7.0.5"
, "org.scalaz"                       %% "scalaz-effect"      % "7.0.5"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

