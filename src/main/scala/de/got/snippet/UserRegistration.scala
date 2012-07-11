package de.got.snippet

import net.liftweb.common._
import net.liftweb.mapper._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.proto.ProtoRules
import Helpers._
import de.got.model._
import S._
import de.got.main._
import java.text.SimpleDateFormat
import net.liftweb.http.js._
import org.joda.time.Years
import org.joda.time.DateTime
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import scala.xml.Elem
import de.got.lib.AjaxFactory._

class UserRegistration extends StatefulSnippet with Logger {

  object RegistrationMethod extends Enumeration {
    val PrivateUser = Value(S ? "private.user")
    val BusinessUser = Value(S ? "business.user")
  }

  val registrationMethodRadios = SHtml.ajaxRadio[RegistrationMethod.Value](RegistrationMethod.values.toList,
    Full(RegistrationMethod.PrivateUser),
    loadTemplateFor(_))

  val germanDate = new SimpleDateFormat("dd.MM.yyyy")

  var r_user = User.create

  var userRetypeEmail = ""
  var userPassword = ""
  var userRetypePassword = ""
  var userAgb = false

  var templateParent = NodeSeq.Empty
  var templateUser = NodeSeq.Empty
  var templateBusiness = NodeSeq.Empty

  def dispatch = _ match {
    case "render" => render
  }

  def render(in: NodeSeq): NodeSeq = {

    templateParent = { "#input-child ^^" #> { (n: NodeSeq) => n } }.apply(in)
    templateUser = { "#register-user ^^" #> { (n: NodeSeq) => n } }.apply(in)
    templateBusiness = { "#register-business ^^" #> { (n: NodeSeq) => n } }.apply(in)

    {
      ".registrationtype" #> registrationMethodRadios.map(item => <span>{ item.xhtml }{ item.key.toString }</span>) &
        "#register-content *" #> renderUserRegistration &
        ".submit" #> SHtml.submit(S ? "register", processRegister)
    }.apply(in)
  }

  def loadTemplateFor(method: RegistrationMethod.Value): JsCmd = {
    method match {
      case RegistrationMethod.PrivateUser =>
        List(JsCmds.SetHtml("register-content", S.eval(renderUserRegistration).openOr(<span>Error while processing Template</span>)),
          JsCmds.Run("birthdayDatepicker()"))
      case RegistrationMethod.BusinessUser => List(JsCmds.SetHtml("register-content", S.eval(renderBusinessRegistration).openOr(<span>Error while processing Template</span>)))
      case _ => JsCmds.Noop
    }
  }

  def checkBirthday(s: String): JsCmd = {
    println(s)
    tryo({
      r_user.birthday(germanDate.parse(s))
      val age = getCurrentAge
        println(age)
      if (age < 18 && age >= 0) {
        JsCmds.SetHtml("input-register-parent", renderParentFields)
      } else {
        JsCmds.SetHtml("input-register-parent", NodeSeq.Empty)
      }

    }).openOr(JsCmds.Noop)
  }

  def formatDate(d: java.util.Date): String = d match { case null => "" case s => germanDate.format(s) }

  def renderUserRegistration: NodeSeq = {
    {
      ".name" #> SHtml.text(r_user.name, r_user.name(_)) &
        ".email" #> SHtml.text(r_user.email, r_user.email(_)) &
        ".retypeemail" #> SHtml.text(userRetypeEmail, userRetypeEmail = _) &
        ".password" #> SHtml.password("", userPassword = _) &
        ".retypepassword" #> SHtml.password("", userRetypePassword = _) &
        ".firstname" #> SHtml.text(r_user.firstName, r_user.firstName(_)) &
        ".lastname" #> SHtml.text(r_user.lastName, r_user.lastName(_)) &
        ".birthday" #> ajaxLiveText(formatDate(r_user.birthday), checkBirthday(_)) &
        { if (getCurrentAge < 18 && getCurrentAge >= 0) cssSelParentFelds else {"#input-register-parent *" #> ""} } &
        ".phone" #> SHtml.text(r_user.phoneNumber, r_user.phoneNumber(_)) &
        ".street" #> SHtml.text(r_user.street, r_user.street(_)) &
        ".postalcode" #> SHtml.text(r_user.postalCode, r_user.postalCode(_)) &
        ".city" #> SHtml.text(r_user.city, r_user.city(_)) &
        ".newsletter" #> SHtml.checkbox(r_user.wantsNewsletter, r_user.wantsNewsletter(_)) &
        ".agb" #> SHtml.checkbox(userAgb, userAgb = _)
    }.apply(templateUser)
  }

  def renderBusinessRegistration: NodeSeq = {
    {
      ".tmp" #> ""
    }.apply(templateBusiness)
  }

  def renderParentFields: NodeSeq = {
    cssSelParentFelds.apply(templateParent)
  }

  def cssSelParentFelds =
    ".parentfirstname" #> SHtml.text(r_user.parentFirstName, r_user.parentFirstName(_)) &
      ".parentlastname" #> SHtml.text(r_user.parentLastName, r_user.parentLastName(_))

  def processRegister() = {
    var invalid = false

    if (r_user.name.length < 3 || r_user.name.length > 20) {
      S.error(S ? "invalid.name.length")
      invalid = true
    }

    if (!r_user.name.matches("[a-zA-Z0-9 ]+")) {
      S.error(S ? "invalid.chars.in.name")
      invalid = true
    }
    
    if(!r_user.email.equals(userRetypeEmail)) {
      S.error(S ? "emails.do.not.match")
      invalid = true
    }

    if (!ProtoRules.emailRegexPattern.vend.matcher(r_user.email).matches()) {
      S.error(S ? "invalid.email")
      invalid = true
    }

    if (!User.findAll(By(User.name, r_user.name)).isEmpty) {
      S.error(S ? "name.allready.in.use")
      invalid = true
    }

    if (!User.findAll(By(User.email, r_user.email)).isEmpty) {
      S.error(S ? "email.allready.in.use")
      invalid = true
    }
    
    if(userPassword.length() < 6) {
      S.error(S ? "password.too.short")
      invalid = true
    }

    if (!userPassword.equals(userRetypePassword)) {
      S.error(S ? "passwords.do.not.match")
      invalid = true
    }
    
    if (!r_user.birthday.toString.isEmpty() && getCurrentAge < 0) {
      S.error(S ? "birthday.in.future")
      invalid = true;
    }

    if (!userAgb) {
      S.error(S ? "agb.had.to.be.accepted")
      invalid = true
    }

    if (!invalid) {
      r_user.password(userPassword)
      if (!r_user.save) {
        S.error(S ? "user.cant.be.saved")
        error("Couldn't save user (%s, %s)".format(r_user.name, r_user.email))
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

  def getCurrentAge = tryo { Years.yearsBetween(new DateTime(r_user.birthday.is), new DateTime(now)).getYears() }.openOr(0)
}