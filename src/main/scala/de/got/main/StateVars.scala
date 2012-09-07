package de.got.main
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import S._
import de.got.model.User

object curUser extends SessionVar[Box[User]](Empty)
object logginName extends SessionVar[Box[String]](Empty)
object curUserIsAdmin extends SessionVar[Box[Boolean]](Empty)