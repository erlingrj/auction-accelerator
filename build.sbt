// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.erlingrj"

lazy val root = (project in file("."))
  .settings(
    name := "auction-accelerator",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.4.2",
      "edu.berkeley.cs" %% "chiseltest" % "0.3.1" % "test",
      "edu.berkeley.cs" %% "chisel-iotesters" % "1.4.1+"
    ),
    scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    ),
    //addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.1" cross CrossVersion.full),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  ).dependsOn(fpgatidbits)

lazy val fpgatidbits = (project in file("./fpga-tidbits"))
  .settings(
name := "fpga-tidbits",
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.4.2" ,
  "edu.berkeley.cs" %% "chiseltest" % "0.3.1" % "test",
  "edu.berkeley.cs" %% "chisel-iotesters" % "1.4.1+"
),
scalacOptions ++= Seq(
  "-Xsource:2.11",
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit"
),
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.1" cross CrossVersion.full),
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  )

// add fpga-tidbits as unmanaged source dependency, pulled as git submodule
//unmanagedSourceDirectories in Compile += baseDirectory.value / "fpga-tidbits" / "src" / "main" / "scala"
// fpga-tidbits stores compile scripts, drivers etc. in the resource dir
//unmanagedResourceDirectories in Compile += baseDirectory.value / "fpga-tidbits" / "src" / "main" / "resources"
