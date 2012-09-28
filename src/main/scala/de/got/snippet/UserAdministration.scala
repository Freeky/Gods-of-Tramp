package de.got.snippet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.proto._
import scala.xml._
import Helpers._
import net.liftweb.common._
import net.liftweb.mapper._
import java.sql.Timestamp
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmd
import de.got.model._
import java.text.SimpleDateFormat

class UserAdministration extends StatefulSnippet {
  def dispatch = _ match {
    case "short" => short
    case "overview" => overview
    case "edituser" => editUser
  }

  val germanDate = new SimpleDateFormat("dd.MM.yyyy")
  
  def short(in: NodeSeq): NodeSeq = {
    bind("short", in,
      "overall" -> Text(User.count.toString()))

  }

  def overview(in: NodeSeq): NodeSeq = {
    var entrycount = 25
    var page = 0
    
    var entryTemplate = {".entry ^^" #> (n => n)}.apply(in)

    def buildUserTable(entries: List[User], template: NodeSeq) = {
      entries.flatMap(entry =>
        {".id" #> entry.id.toString &
          ".name" #> entry.name.is &
          ".type" #> entry.accountType.obj.map(_.name.is).openOr("unkown") &
          ".editlink [href]" #> ("/admin/users/%d".format(entry.id.is))
          }.apply(entryTemplate)
      )
    }

    def userTable() = {
      val entries = User.findAll(StartAt(page * entrycount), MaxRows(entrycount))
      buildUserTable(entries.toList, in)
    }

    def updateUserTable(): JsCmd = {
      List(JsCmds.SetHtml("user_table", userTable),
        JsCmds.SetHtml("current_page", Text((page + 1).toString)))
    }

    def updateEntryCount(e: String) = {
      entrycount = Integer.parseInt(e)
      page = 0
      updateUserTable
    }

    def prevPage = {
      if (page > 0) page = page - 1
      updateUserTable
    }

    def nextPage = {
      val max = User.count
      if (((page + 1) * entrycount) < max) page = page + 1
      updateUserTable
    }

    {".entrycount" #> SHtml.ajaxSelect(List(10, 25, 50, 100).map(i => (i.toString, i.toString)),
        Full(25.toString), v => updateEntryCount(v)) &
      ".page" #> Text((page + 1).toString) &
      ".prevpage" #> SHtml.ajaxButton(Text(S ? "previous"), () => prevPage) &
      ".nextpage" #> SHtml.ajaxButton(Text(S ? "next"), () => nextPage) &
      "#user_table" #> userTable}.apply(in)
  }

  def editUser = {
    var password = ""
    var passwordretype = ""
    var accountType = ""
    val userOption = S.param("id") match {
      case Full(idStr) => {
        User.find(By(User.id, idStr.toLong))
      }
      case _ => Empty
    }

    def buildAccountTypeValues(): List[(String, String)] = {
      AccountType.findAll().map(at => (at.id.toString, at.name.is))
    }

    def processEditUser() = {
      userOption match {
        case Full(user) => {
          // new password
          if (password.length > 0 || passwordretype.length > 0) {
            if (password.equals(passwordretype)) {
              user.password(password)
            } else {
              S.error(S ? "passwords.do.not.match")
            }
          }

          // new accounttype
          user.accountType(accountType.toLong)

          user.save()

        }
        case _ => ()
      }
    }

    userOption match {
      case Full(user) => {
          ".name" #> SHtml.text(user.name, user.name(_)) &
          ".password" #> SHtml.password(password, password = _) &
          ".passwordretype" #> SHtml.password(passwordretype, passwordretype = _) &
          ".email" #> SHtml.text(user.email, user.email(_)) &
          ".registrationdate" #> Text(user.registrationDate.toString()) &
          ".accounttype" #> SHtml.select(buildAccountTypeValues, Full(user.accountType.is.toString), accountType = _) &
          ".newsletter" #> SHtml.checkbox(user.wantsNewsletter, user.wantsNewsletter(_)) &
          ".firstname" #> SHtml.text(user.firstName, user.firstName(_)) &
          ".lastname" #> SHtml.text(user.lastName, user.lastName(_)) &
          ".birthday" #> SHtml.text(tryo{germanDate.format(user.birthday.is)}.openOr(""), s => tryo {user.birthday(germanDate.parse(s))}) &
          ".parentfirstname" #> SHtml.text(user.parentFirstName, user.parentLastName(_)) &
          ".parentlastname" #> SHtml.text(user.parentLastName, user.parentLastName(_)) &
          ".street" #> SHtml.text(user.street, user.street(_)) &
          ".postalcode" #> SHtml.text(user.postalCode, user.postalCode(_)) &
          ".city" #> SHtml.text(user.city, user.city(_)) &
          ".phonenumber" #> SHtml.text(user.phoneNumber, user.phoneNumber(_)) &
          ".submit" #> SHtml.submit(S ? "edit", processEditUser)
      }
      case _ => "*" #> ""
    }
  }
}