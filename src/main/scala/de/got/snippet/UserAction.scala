package de.got.snippet
import net.liftweb.common._
import net.liftweb.mapper._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.util.Helpers
import net.liftweb.http._
import net.liftweb.proto.ProtoRules
import js.JsCmds._
import Helpers._
import de.got.model._
import S._
import de.got.main._

class UserAction extends StatefulSnippet with Logger {

  var userName = ""
  var userEmail = ""
  var userRetypeEmail = ""
  var userPassword = ""
  var userRetypePassword = ""
  var firstName = ""
  var lastName = ""
  var birthday = ""
  var parentFirstName = ""
  var parentLastName = ""
  var phoneNumber = ""
  var street = ""
  var postalCode = ""
  var city = ""
  var userNewsletter = false
  var userAgb = false

  def dispatch: DispatchIt = _ match {
    case "login" => login
    case "register" => register
    case "panel" => panel
    case "changepassword" => changePassword
    case "changemail" => changeEmail
    case "changenewsletter" => changeNewsletter
    case "deleteaccount" => deleteAccount
  }

  def login = {

    def processLogin() = {
      val userInABox = for {
        foundUser <- User.find(By(User.email, userEmail)) ?~ "The User was not found"
        if foundUser.password.match_?(userPassword)
      } yield foundUser

      if (userInABox.isDefined) {
        val user = userInABox.open_!
        if (user.accountType.obj.open_!.isAdmin.is == true)
          curUserIsAdmin(Full(true))
        curUserId(Full(user.id.is))
        logginName(Full(user.name.is))
        unregisterThisSnippet()
        S.redirectTo("/")
      } else S.error(S ? "user.or.password.wrong")
    }

    if (!User.loggedIn_?()) {
      ".email" #> SHtml.text(userEmail, userEmail = _) &
        ".password" #> SHtml.password("", userPassword = _) &
        ".submit" #> SHtml.submit(S ? "login", processLogin)
    } else
      "*" #> <span>{ S ? "allready.logged.in" }</span>
  }

  def register = {

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

      if (!userAgb) {
        S.error(S ? "agb.had.to.be.accepted")
        invalid = true
      }

      if (!invalid) {
        var user = new User().name(userName).password(userPassword).email(userEmail).wantsNewsletter(userNewsletter)
        if (!User.save(user)) {
          S.error(S ? "user.cant.be.saved")
          error("Couldn't save user (%s, %s)".format(userName, userEmail))
        } else {
          unregisterThisSnippet()
          S.notice(S ? "successfull.registration")
          S.redirectTo("/")
        }
      } else {
        userPassword = ""
        userRetypePassword = ""
      }
    }

    ".name" #> SHtml.text(userName, userName = _) &
      ".email" #> SHtml.text(userEmail, userEmail = _) &
      ".retypeemail" #> SHtml.text(userRetypeEmail, userRetypeEmail = _) &
      ".password" #> SHtml.password("", userPassword = _) &
      ".retypepassword" #> SHtml.password("", userRetypePassword = _) &
      ".firstname" #> SHtml.text(firstName, firstName = _) &
      ".lastname" #> SHtml.text(lastName, lastName = _) &
      ".birthday" #> SHtml.text(birthday, birthday = _) &
      ".parentfirstname" #> SHtml.text(parentFirstName, parentFirstName = _) &
      ".parentlastname" #> SHtml.text(parentLastName, parentLastName = _) &
      ".phone" #> SHtml.text(phoneNumber, phoneNumber = _) &
      ".street" #> SHtml.text(street, street = _) &
      ".postalcode" #> SHtml.text(postalCode, postalCode = _) &
      ".city" #> SHtml.text(city, city = _) &
      ".newsletter" #> SHtml.checkbox(userNewsletter, userNewsletter = _) &
      ".agb" #> SHtml.checkbox(userAgb, userAgb = _) &
      ".submit" #> SHtml.submit(S ? "register", processRegister)
  }

  def processLogout() = {
    curUserIsAdmin(Empty)
    curUserId(Empty)
    logginName(Empty)
    unregisterThisSnippet()
    S.redirectTo("/")
  }

  def panel = {

    if (User.loggedIn_?()) {
      ".name" #> logginName.openOr("Unknown") &
        ".options" #> <lift:Menu.item name="options"/> &
        ".logout" #> SHtml.submit(S ? "logout", processLogout)
    } else {
      "*" #> <div><lift:Menu.item name="login"/> <lift:Menu.item name="register"/></div>
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

  def changePassword = {
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

            unregisterThisSnippet()
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
        ".currentpassword" #> SHtml.password(currentPassword, currentPassword = _) &
          ".newpassword" #> SHtml.password(newPassword, newPassword = _) &
          ".newpasswordretype" #> SHtml.password(repeatNewPassword, repeatNewPassword = _) &
          ".submit" #> SHtml.submit(S ? "change", () => processChangePassword)
      }
      case _ => {
        warn("changePassword-snippet was invoked but no user was set")
        "*" #> Text("There was an error")
      }
    }
  }

  def changeEmail = {

    def processChangeEmail = {
      User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
        case Full(user) => {
          user.email(userEmail).save()
        }
        case _ => S.error(S ? "no.permission")
      }
    }

    User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
      case Full(user) => {
        ".email" #> SHtml.text(userEmail, userEmail = _) &
          ".submit" #> SHtml.submit(S ? "change", () => processChangeEmail)
      }
      case _ => {
        warn("changeEmail-snippet was invoked but no user was set")
        "*" #> Text("There was an error")
      }
    }
  }

  def changeNewsletter = {

    def processChangeNewsletter = {
      User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
        case Full(user) => {
          user.wantsNewsletter(userNewsletter).save()
        }
        case _ => S.error(S ? "no.permission")
      }
    }

    User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
      case Full(user) => {
        userNewsletter = user.wantsNewsletter
        ".newsletter" #> SHtml.checkbox(userNewsletter, userNewsletter = _) &
          ".submit" #> SHtml.submit(S ? "change", () => processChangeNewsletter)
      }
      case _ => {
        warn("changeEmail-snippet was invoked but no user was set")
        "*" #> Text("There was an error")
      }
    }
  }

  def deleteAccount = {
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
      S.redirectTo("/options")
    }

    ".yes" #> SHtml.submit(S ? "yes", processYes) &
      ".no" #> SHtml.submit(S ? "no", processNo)
  }
}