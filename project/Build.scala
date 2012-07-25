import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "de.got"
  val buildVersion      = "0.2.3"
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object Resolvers {
  val sunrepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
  val sunrepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish" 
  val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
  val jettyRepo  = "Jetty Repo" at "http://repo1.maven.org/maven2/org/mortbay/jetty"
  val boneCPRepo = "BoneCP Repository" at "http://jolbox.com/bonecp/downloads/maven"

  val oracleResolvers = Seq (sunrepo, sunrepoGF, oraclerepo)
}

object Dependencies {
  val logbackVer = "0.9.26"
  val liftVersion = "2.4"

  val liftwebkit     = "net.liftweb"             %% "lift-webkit"          % liftVersion % "compile->default"
  val liftmapper	 = "net.liftweb" 			 %% "lift-mapper" 		   % liftVersion % "compile->default"
  val lifttextile	 = "net.liftweb" 			 %% "lift-textile" 		   % liftVersion % "compile->default"
  val jetty          = "org.mortbay.jetty"       %  "jetty"                % "6.1.26"    % "container,test"
  val servlet        = "javax.servlet"           %  "servlet-api"          % "2.5"       % "provided->default"
  val junit          = "junit"                   %  "junit"                % "4.7"       % "test"
  val scalatest      = "org.scala-tools.testing" %  "specs_2.9.0"          % "1.6.8"     % "test"
  val logbackcore    = "ch.qos.logback"          %  "logback-core"         % logbackVer
  val logbackclassic = "ch.qos.logback"          %  "logback-classic"      % logbackVer
  val mysql          = "mysql"                   %  "mysql-connector-java" % "5.1.20"
}

object FreekyWeb extends Build {

	import Resolvers._
	import Dependencies._
	import BuildSettings._
	import com.github.siasia.WebPlugin.webSettings
	
	// Sub-project specific dependencies
	  val liftDeps = Seq (
		liftwebkit,
		liftmapper,
		lifttextile,
		jetty,
		servlet,
		junit,
		scalatest,
		logbackcore,
		logbackclassic,
		mysql
	  )
	    
	lazy val root = Project(
		"root", file("."),
		settings = buildSettings ++ Seq (libraryDependencies ++= liftDeps) ++ webSettings
	)
}
