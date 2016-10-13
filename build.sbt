name := "testing" 

version := "1.0"

scalaVersion := "2.10.4"

//resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
//                  "releases"  at "http://scala-tools.org/repo-releases")

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.6" % "test",
                            "joda-time" % "joda-time" % "1.6.2",
                            "junit" % "junit" % "4.10",
                            "org.testng" % "testng" % "6.9.10" % "test",
                            "org.specs2" %% "specs2" % "2.3.10",
                            "org.specs2" %% "specs2-scalaz-core" % "7.0.0" % "test",
                            "org.easymock" % "easymock" % "3.1" % "test",
                            "org.mockito" % "mockito-core" % "1.9.0" % "test",
                            "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
                            "org.scalacheck" %% "scalacheck" % "1.13.2" % "test")
