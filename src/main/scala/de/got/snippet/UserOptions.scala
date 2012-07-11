package de.got.snippet

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.mapper._
import scala.xml._
import Helpers._
import de.got.model._
import java.text.SimpleDateFormat
import net.liftweb.http.js._
import org.joda.time.Years
import org.joda.time.DateTime
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import scala.xml.Elem
import net.liftweb.proto.ProtoRules
import de.got.lib.AjaxFactory._

class UserOptions extends StatefulSnippet with Logger {

  def dispatch() = _ match {
    case "render" => render
  }

  val germanDate = new SimpleDateFormat("dd.MM.yyyy")

  val currentUser = User.find(By(User.id, User.currentUserId.openOr(-1L)))

  var templateParent = NodeSeq.Empty

  def render(in: NodeSeq): NodeSeq = {
    templateParent = { "#input-child ^^" #> { (n: NodeSeq) => n } }.apply(in)

    currentUser match {
      case Full(user) => {
        ".name" #> Text(user.name) &
          ".firstname" #> SHtml.text(user.firstName, user.firstName(_)) &
          ".lastname" #> SHtml.text(user.lastName, user.lastName(_)) &
          ".birthday" #> ajaxLiveText(formatDate(user.birthday), checkBirthday(_)) &
          { if (getCurrentAge < 18 && getCurrentAge >= 0) cssSelParentFelds else { "#input-options-parent *" #> "" } } &
          ".phone" #> SHtml.text(user.phoneNumber, user.phoneNumber(_)) &
          ".street" #> SHtml.text(user.street, user.street(_)) &
          ".postalcode" #> SHtml.text(user.postalCode, user.postalCode(_)) &
          ".city" #> SHtml.text(user.city, user.city(_)) &
          ".newsletter" #> SHtml.checkbox(user.wantsNewsletter, user.wantsNewsletter(_)) &
          ".registrationdate" #> Text(formatDate(user.registrationDate)) &
          ".mail" #> Text(user.email) &
          ".changemail" #> <lift:Menu.item name="changemail"/> &
          ".changepassword" #> <lift:Menu.item name="changepassword"/> &
          ".deleteaccount" #> <lift:Menu.item name="deleteaccount"/> &
          ".submit" #> SHtml.submit(S ? "change", processOptions) &
          ".fulluserinfo" #> Text(if(User.isFullUser_?(user)) {S ? "yes"} else {S ? "no"})
      }.apply(in)
      case _ => {
        warn("show-snippet was invoked but no user was set")
        ("*" #> "There was an error").apply(in)
      }
    }
  }

  def renderParentFields: NodeSeq = {
    cssSelParentFelds.apply(templateParent)
  }

  def cssSelParentFelds =
    ".parentfirstname" #> SHtml.text(currentUser.open_!.parentFirstName, currentUser.open_!.parentFirstName(_)) &
      ".parentlastname" #> SHtml.text(currentUser.open_!.parentLastName, currentUser.open_!.parentLastName(_))

  def processOptions() = {
        println("is called")
    var invalid = false

    currentUser match {
      case Full(user) => {

        if (!user.birthday.toString.isEmpty() && getCurrentAge < 0) {
          S.error(S ? "birthday.in.future")
          invalid = true;
        }

      }
      case _ => {
        S.error(S ? "no.valid.user")
        invalid = true
      }
    }

    if (!invalid) {
      currentUser.map(user => {
        user.save()
        S.notice(S ? "chganes.saved")
      })
    }
  }

  def formatDate(d: java.util.Date): String = d match { case null => "" case s => germanDate.format(s) }

  def checkBirthday(s: String): JsCmd = {
    tryo({
      if(s.isEmpty()) {
        currentUser.open_!.birthday(null)
      } else
      currentUser.open_!.birthday(germanDate.parse(s))
      val age = getCurrentAge
      println(age)
      if (age < 18 && age >= 0) {
        JsCmds.SetHtml("input-options-parent", renderParentFields)
      } else {
        JsCmds.SetHtml("input-options-parent", NodeSeq.Empty)
      }

    }).openOr(JsCmds.Noop)
  }

  def getCurrentAge = tryo { Years.yearsBetween(new DateTime(currentUser.open_!.birthday.is), new DateTime(now)).getYears() }.openOr(0)

}