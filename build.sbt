organization := "com.mao.zhehao"

version := "1.0"

name := "soundkit"

scalaVersion := "2.11.6"

libraryDependencies +=
  ("edu.berkeley.cs" %% "chisel" % System.getProperty("chiselVersion", ""))

libraryDependencies +=
  ("com.mao.zhehao" %% "codec" % System.getProperty("codecVersion", ""))
