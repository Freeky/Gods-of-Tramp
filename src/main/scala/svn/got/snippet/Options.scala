package svn.got.snippet

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.mapper._
import scala.xml._
import Helpers._

import svn.got.model._

class Options extends DispatchSnippet with Logger {
  def dispatch() = _ match {
    case "show" => show
  }

  def show(in: NodeSeq): NodeSeq = {
    User.find(By(User.id, User.currentUserId.openOr(-1L))) match {
      case Full(user) => {
        bind("show", in,
          "registrationdate" -> Text(user.registrationDate.toString()),
          "mail" -> Text(user.email),
          "newsletter" -> Text(if(user.wantsNewsletter) {S ? "yes"} else {S ? "no"}),
          "changemail" -> <lift:Menu.item name="changemail"/>,
          "changepassword" -> <lift:Menu.item name="changepassword"/>,
          "changenewsletter" -> <lift:Menu.item name="changenewsletter"/>,
          "deleteaccount" -> <lift:Menu.item name="deleteaccount"/>)
      }
      case _ => {
        warn("show-snippet was invoked but no user was set")
        Text("There was an error")
      }
    }
  }

}