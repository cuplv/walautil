name := "WalaUtil"

version := "0.1-SNAPSHOT"

organization := "University of Colorado Boulder"

scalaVersion := "2.10.2"

offline := true

libraryDependencies ++= Seq(
	"com.ibm.wala" % "com.ibm.wala.shrike" % "1.3.7",
	"com.ibm.wala" % "com.ibm.wala.util" % "1.3.7",
	"com.ibm.wala" % "com.ibm.wala.core" % "1.3.7")
