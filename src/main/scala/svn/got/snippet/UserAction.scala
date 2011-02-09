package svn.got.snippet
import net.liftweb.common.Full

import net.liftweb.common.Failure
import net.liftweb.mapper._
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import Helpers._
import svn.got.model._
import S._
import svn.got.main._

class UserAction extends StatefulSnippet {
  def dispatch = _ match {
    case "login" => login
  }

  def login(in: NodeSeq): NodeSeq = {
    var userName = ""
    var userPassword = ""

    def processLogin() = {
      val userInABox = for {
        foundUser <- User.find(By(User.name, userName)) ?~ "The User was not found"
        if foundUser.password.match_?(userPassword)
      } yield foundUser
      
      if(userInABox.isDefined){
    	  val user = userInABox.open_!
    	  if(user.isAdmin.is == true)
    	 	  curUserIsAdmin(Full(true))
    	  curUserId(Full(user.id.is))
    	  logginName(Full(user.name.is))
    	  S.redirectTo("/admin")
      } else S.error(S ? "user.or.password.wrong")
    }

    bind("form", in,
      "name" -> SHtml.text(userName, userName = _),
      "password" -> SHtml.password(userPassword, userPassword = _),
      "submit" -> SHtml.submit(S ? "login", processLogin))
  }
}