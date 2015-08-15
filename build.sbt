organization := "org.soundkit"

version := "1.0"

name := "soundkit"

scalaVersion := "2.11.6"

libraryDependencies +=
  ("edu.berkeley.cs" %% "chisel" % System.getProperty("chiselVersion", ""))

libraryDependencies +=
  ("org.soundkit" %% "codec" % System.getProperty("codecVersion", ""))
