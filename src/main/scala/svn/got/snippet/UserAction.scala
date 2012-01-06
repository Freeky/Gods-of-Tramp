package svn.got.snippet
import net.liftweb.common._
import net.liftweb.mapper._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.util.Helpers
import net.liftweb.http._
import net.liftweb.proto.ProtoRules
import Helpers._
import svn.got.model._
import S._
import svn.got.main._

class UserAction extends StatefulSnippet with Logger {
  def dispatch = _ match {
    case "login" => login
    case "register" => register
    case "panel" => panel
  }

  def login(in: NodeSeq): NodeSeq = {
    var userName = ""
    var userPassword = ""

    def processLogin() = {
      val userInABox = for {
        foundUser <- User.find(By(User.name, userName)) ?~ "The User was not found"
        if foundUser.password.match_?(userPassword)
      } yield foundUser

      if (userInABox.isDefined) {
        val user = userInABox.open_!
        if (user.isAdmin.is == true)
          curUserIsAdmin(Full(true))
        curUserId(Full(user.id.is))
        logginName(Full(user.name.is))
        S.redirectTo("/")
      } else S.error(S ? "user.or.password.wrong")
    }

    if (!User.loggedIn_?())
      bind("form", in,
        "name" -> SHtml.text(userName, userName = _),
        "password" -> SHtml.password(userPassword, userPassword = _),
        "submit" -> SHtml.submit(S ? "login", processLogin))
    else
      <span>{ S ? "allready.logged.in" }</span>
  }

  def register(in: NodeSeq): NodeSeq = {

    var userName = ""
    var userEmail = ""
    var userPassword = ""
    var userRetypePassword = ""

    def processRegister() = {
      var invalid = false

      if (userName.length < 3 || userName.length > 20) {
        S.error(S ? "invalid.name.length")
        invalid = true
      }

      if (!userName.matches("[a-zA-Z0-9]+")) {
        S.error(S ? "invalid.chars.in.name")
        invalid = true
      }

      if (!ProtoRules.emailRegexPattern.vend.matcher(userEmail).matches()) {
        S.error(S ? "invalid.email")
        invalid = true
      }

      if (!User.findAll(By(User.name, userName)).isEmpty) {
        S.error(S ? "name.allready.in.use")
        invalid = true
      }

      if (!User.findAll(By(User.email, userEmail)).isEmpty) {
        S.error(S ? "email.allready.in.use")
        invalid = true
      }

      if (!userPassword.equals(userRetypePassword)) {
        S.error(S ? "passwords.do.not.match")
        invalid = true
      }

      if (!invalid) {
        var user = new User().name(userName).password(userPassword).email(userEmail)
        if (!User.save(user)) {
          S.error(S ? "user.cant.be.saved")
          error("Couldn't save user (%s, %s)".format(userName, userEmail))
        } else {
          S.notice(S ? "successfull.registration")
          S.redirectTo("/")
        }
      } else {
        userPassword = ""
        userRetypePassword = ""
      }
    }

    bind("form", in,
      "name" -> SHtml.text(userName, userName = _),
      "email" -> SHtml.text(userEmail, userEmail = _),
      "password" -> SHtml.password(userPassword, userPassword = _),
      "retypepassword" -> SHtml.password(userRetypePassword, userRetypePassword = _),
      "submit" -> SHtml.submit(S ? "register", processRegister))
  }

  def panel(in: NodeSeq): NodeSeq = {

    def processLogout() = {
      curUserIsAdmin(Empty)
      curUserId(Empty)
      logginName(Empty)
      S.redirectTo("/")
    }

    if (User.loggedIn_?()) {
      bind("form", in,
        "name" -> logginName.openOr("Unknown"),
        "logout" -> SHtml.submit(S ? "logout", processLogout))
    } else {
      <lift:Menu.item name="login"/>
    }
  }
}