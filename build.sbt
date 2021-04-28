// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.1"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "NTNU"

lazy val root = (project in file("."))
  .settings(
    name := "auction-accelerator",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.3",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.3",
      "edu.berkeley.cs" %% "chisel-iotesters" % "1.4.1+"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:useBundlePlugin"
),
    testOptions ++= Seq(
//      Tests.Argument("-oF") // Dont truncate stack trace
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.3" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  ).dependsOn(fpgatidbits)

// Import fpgatidbits project
lazy val fpgatidbits = (project in file("./fpga-tidbits"))
