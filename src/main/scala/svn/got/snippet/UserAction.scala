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

class UserAction extends DispatchSnippet with Logger {
  def dispatch = _ match {
    case "login" => login
    case "register" => register
    case "panel" => panel
    case "changepassword" => changePassword
    case "changemail" => changeEmail
    case "changenewsletter" => changeNewsletter
    case "deleteaccount" => deleteAccount
  }

  def login(in: NodeSeq): NodeSeq = {
    var userEmail = ""
    var userPassword = ""

    def processLogin() = {
      val userInABox = for {
        foundUser <- User.find(By(User.email, userEmail)) ?~ "The User was not found"
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
        "email" -> SHtml.text(userEmail, userEmail = _),
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
    var userNewsletter = false
    var agb = false

    def processRegister() = {
      var invalid = false

      if (userName.length < 3 || userName.length > 20) {
        S.error(S ? "invalid.name.length")
        invalid = true
      }

      if (!userName.matches("[a-zA-Z0-9 ]+")) {
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
      
      if(!agb) {
        S.error(S ? "agb.had.to.be.accepted")
        invalid = true
      }

      if (!invalid) {
        var user = new User().name(userName).password(userPassword).email(userEmail).wantsNewsletter(userNewsletter)
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
      "newsletter" -> SHtml.checkbox(userNewsletter, userNewsletter = _),
      "agb" -> SHtml.checkbox(agb, agb = _),
      "submit" -> SHtml.submit(S ? "register", processRegister))
  }
  
  def processLogout() = {
      curUserIsAdmin(Empty)
      curUserId(Empty)
      logginName(Empty)
      S.redirectTo("/")
    }

  def panel(in: NodeSeq): NodeSeq = {

    if (User.loggedIn_?()) {
      bind("form", in,
        "name" -> logginName.openOr("Unknown"),
        "options" -> <lift:Menu.item name="options"/>,
        "logout" -> SHtml.submit(S ? "logout", processLogout))
    } else {
      <div><lift:Menu.item name="login"/> <lift:Menu.item name="register"/></div>
    }
  }

  def checkNewPassword(pw: String, pwr: String): List[String] = {
    var errors = List[String]()

    if (!pw.equals(pwr)) {
      errors = (S ? "passwords.do.not.match") :: errors
    }
    if (!(pw.length() >= 6)) {
      errors = (S ? "password.too.short") :: errors
    }

    errors
  }

  def changePassword(in: NodeSeq): NodeSeq = {
    var currentPassword = ""
    var newPassword = ""
    var repeatNewPassword = ""

    def processChangePassword = {
      var errors = List[String]()
      User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
        case Full(user) => {
          if (!user.password.match_?(currentPassword)) {
            errors = (S ? "passwords.do.not.match") :: errors
          }
          errors = checkNewPassword(newPassword, repeatNewPassword) ::: errors

          if (errors.isEmpty) {
            user.password(newPassword).save()

            S.notice(S ? "new.password.set")
            S.redirectTo("/options")
          } else {
            errors.foreach(S.error(_))
          }
        }
        case _ => S.error(S ? "no.permission")
      }
    }

    User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
      case Full(user) => {
        bind("changepassword", in,
          "currentpassword" -> SHtml.password(currentPassword, currentPassword = _),
          "newpassword" -> SHtml.password(newPassword, newPassword = _),
          "newpasswordretype" -> SHtml.password(repeatNewPassword, repeatNewPassword = _),
          "submit" -> SHtml.submit(S ? "change", () => processChangePassword))
      }
      case _ => {
        warn("changePassword-snippet was invoked but no user was set")
        Text("There was an error")
      }
    }
  }

  def changeEmail(in: NodeSeq): NodeSeq = {
    var email = ""

    def processChangeEmail = {
      User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
        case Full(user) => {
          user.email(email).save()
        }
        case _ => S.error(S ? "no.permission")
      }
    }

    User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
      case Full(user) => {
        bind("changemail", in,
          "email" -> SHtml.text(email, email = _),
          "submit" -> SHtml.submit(S ? "change", () => processChangeEmail))
      }
      case _ => {
        warn("changeEmail-snippet was invoked but no user was set")
        Text("There was an error")
      }
    }
  }

  def changeNewsletter(in: NodeSeq): NodeSeq = {
    var newsletter = false

    def processChangeNewsletter = {
      User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
        case Full(user) => {
          user.wantsNewsletter(newsletter).save()
        }
        case _ => S.error(S ? "no.permission")
      }
    }

    User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
      case Full(user) => {
        newsletter = user.wantsNewsletter
        bind("changenewsletter", in,
          "newsletter" -> SHtml.checkbox(newsletter, newsletter = _),
          "submit" -> SHtml.submit(S ? "change", () => processChangeNewsletter))
      }
      case _ => {
        warn("changeEmail-snippet was invoked but no user was set")
        Text("There was an error")
      }
    }
  }

  def deleteAccount(in: NodeSeq): NodeSeq = {
    def processYes() = {
      User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
        case Full(user) => {
          user.delete_!
          processLogout
        }
        case _ => S.error(S ? "no.permission")
      }
    }
    def processNo() = {
      S.redirectTo("options")
    }
    
    bind("form", in,
        "yes" -> SHtml.submit(S ? "yes", processYes),
        "no" -> SHtml.submit(S ? "no", processNo))
  }
}