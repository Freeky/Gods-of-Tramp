package bootstrap.liftweb
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._
import _root_.net.liftweb.mapper._
import _root_.java.sql._

object DBVendor extends ConnectionManager with Logger {

  println(Props.modeName)
  println(Props.props)
  println(Props)
  println(Props.propFileName)
  
  Props.requireOrDie("db.driver", "db.url", "db.user", "db.password")

  Class.forName(Props.get("db.driver", ""))
  def newConnection(name: ConnectionIdentifier) = {
    try {
      Full(DriverManager.getConnection(
        Props.get("db.url", ""),
        Props.get("db.user", ""), Props.get("db.password", "")))
    } catch {
      case e: Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) { conn.close }

}