organization := "net.degoes.redeyes"

name := "redeyes"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)

resolvers ++= Seq(Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots"))

libraryDependencies ++= Seq(
  "org.scalaz"      %% "scalaz-core"                % "7.0.5",
  "org.scalaz"      %% "scalaz-concurrent"          % "7.0.5",
  "org.threeten"    % "threetenbp"                  % "0.8.1",
  "com.chuusai"     % "shapeless"                   % "2.0.0-M1" cross CrossVersion.full,
  "org.scalaz"      %% "scalaz-scalacheck-binding"  % "7.0.5" % "test",
  "org.scalacheck"  %% "scalacheck"                 % "1.10.1" % "test"
)

seq(bintraySettings:_*)

publishMavenStyle := true

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

bintray.Keys.packageLabels in bintray.Keys.bintray :=
  Seq("restful api", "rest apis", "web framework", "functional programming", "scala")

publishTo <<= (version).apply { v =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("Snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("Releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += {
  Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match {
    case Seq(Some(user), Some(pass)) =>
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    case _ =>
      Credentials(Path.userHome / ".ivy2" / ".credentials")
  }
}

pomIncludeRepository := Function.const(false)

pomExtra := (
  <url>http://github.com/jdegoes/redeyes</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/jdegoes/redeyes</url>
    <connection>scm:git:git://github.com/jdegoes/redeyes.git</connection>
    <developerConnection>scm:git:git@github.com:jdegoes/redeyes.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>jdegoes</id>
      <name>John A. De Goes</name>
      <url>https://degoes.net</url>
    </developer>
  </developers>
)