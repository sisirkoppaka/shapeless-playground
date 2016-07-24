scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1",
  "org.scalactic" %% "scalactic" % "3.0.0-RC4",
  "org.scalatest" %% "scalatest" % "3.0.0-RC4" % "test"
)



scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)
