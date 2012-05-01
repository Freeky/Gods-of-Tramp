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

class UserAdministration extends StatefulSnippet {
  def dispatch = _ match {
    case "short" => short
    case "overview" => overview
    case "edituser" => editUser
  }

  def short(in: NodeSeq): NodeSeq = {
    bind("short", in,
      "overall" -> Text(User.count.toString()))

  }

  def overview(in: NodeSeq): NodeSeq = {
    var entrycount = 25
    var page = 0

    def buildUserTable(entries: List[User], template: NodeSeq) = {
      entries.flatMap({ entry =>
        bind("entry", chooseTemplate("overview", "entry", template),
          "id" -> Text(entry.id.toString),
          "name" -> Text(entry.name),
          "type" -> Text(entry.accountType.obj.open_!.name),
          "editlink" -> { nodes: NodeSeq =>
            <a href={ "/admin/users/" + entry.id.toString }>{ nodes }</a>
          })
      })
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

    bind("overview", in,
      "entrycount" -> SHtml.ajaxSelect(List(10, 25, 50, 100).map(i => (i.toString, i.toString)),
        Full(25.toString), v => updateEntryCount(v)),
      "page" -> Text((page + 1).toString),
      "prevpage" -> SHtml.ajaxButton(Text(S ? "previous"), () => prevPage),
      "nextpage" -> SHtml.ajaxButton(Text(S ? "next"), () => nextPage),
      "table" -> userTable)
  }

  def editUser(in: NodeSeq): NodeSeq = {
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
        bind("user", in,
          "name" -> SHtml.text(user.name, user.name(_)),
          "password" -> SHtml.password(password, password = _),
          "passwordretype" -> SHtml.password(passwordretype, passwordretype = _),
          "email" -> SHtml.text(user.email, user.email(_)),
          "registrationdate" -> Text(user.registrationDate.toString()),
          "accounttype" -> SHtml.select(buildAccountTypeValues, Full(user.accountType.is.toString), accountType = _),
          "newsletter" -> SHtml.checkbox(user.wantsNewsletter, user.wantsNewsletter(_)),
          "submit" -> SHtml.submit(S ? "edit", processEditUser))
      }
      case _ => Text("")
    }
  }
}