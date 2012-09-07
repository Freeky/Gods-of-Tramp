package de.got.snippet
import net.liftweb.common._
import net.liftweb.mapper._
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import Helpers._
import de.got.model._
import S._
import java.text.SimpleDateFormat
import de.got.main._
import net.liftweb.textile._
import net.liftweb.http.js._
import JE._
import JsCmd._
import JsCmds._
import JsExp._
import de.got.lib.DateFunctions._

class CreateAppointment extends DispatchSnippet {
  def dispatch: DispatchIt = _ match {
    case "render" => create
  }

  def create = {

    val appointment = Appointment.create

    def processCreate() = {
      appointment.save
      S.notice(S ? "appointment.created")
      S.redirectTo("/offers/%d".format(appointment.offer.is))
    }

    S.param("id").map(_.toLong) match {
      case Full(offerId) => {

        appointment.offer(offerId)
        appointment.owner(User.currentUserId.openOr(0L))

        ".description" #> SHtml.text(appointment.description, appointment.description(_)) &
          ".from" #> SHtml.text(formatDateTime(appointment.from), s => appointment.from(germanDateTime.parse(s))) &
          ".till" #> SHtml.text(formatDateTime(appointment.till), s => appointment.till(germanDateTime.parse(s))) &
          ".deadline" #> SHtml.text(formatDateTime(appointment.deadline), s => appointment.deadline(germanDateTime.parse(s))) &
          ".spots" #> SHtml.text(appointment.spots.toString, s => appointment.spots(s.toInt)) &
          ".submit" #> SHtml.submit(S ? "add", processCreate)
      }
      case _ => {
        S.error("id.is.empty")
        "*" #> ""
      }
    }

  }

}

class EditAppointment extends StatefulSnippet {
  def dispatch: DispatchIt = _ match {
    case "render" => edit
  }

  val appointmentBox = for {
    id <- S.param("id").map(_.toLong)
    a <- Appointment.find(id)
  } yield a

  def edit = {

    appointmentBox match {
      case Full(appointment) => {

        def buildAttendees =
          AppointmentAttendee.findAll(By(AppointmentAttendee.appointment, appointment)).map(a => {
            ".attendee-id" #> a.id.toString &
              ".attendee-name" #> a.attendeeName &
              ".attendee-status" #> (S.?(a.status.toString)) &
              ".attendee-registration-date" #> formatDate(a.registrationDate)
          })

        ".description" #> SHtml.text(appointment.description, appointment.description(_)) &
          ".from" #> SHtml.text(formatDateTime(appointment.from), s => appointment.from(germanDateTime.parse(s))) &
          ".till" #> SHtml.text(formatDateTime(appointment.till), s => appointment.till(germanDateTime.parse(s))) &
          ".deadline" #> SHtml.text(formatDateTime(appointment.deadline), s => if (!s.equals("")) appointment.deadline(germanDateTime.parse(s))) &
          ".spots" #> SHtml.text(appointment.spots.toString, s => appointment.spots(s.toInt)) &
          ".edit" #> SHtml.submit(S ? "edit", processEdit) &
          ".delete" #> SHtml.submit(S ? "delete", processDelete, "onclick" ->
            JsIf(JsRaw("confirm('%s')".format(S ? "are.you.sure.to.delete")),
              JsReturn(true), JsReturn(false)).toJsCmd) &
          ".attendee-entry" #> buildAttendees
      }
      case _ => {
        S.error("id.is.empty")
        "*" #> ""
      }
    }
  }
  def processEdit() = {
    appointmentBox match {
      case Full(appointment) => {
        appointment.editDate(now).save
        S.notice(S ? "appointment.edited")
        unregisterThisSnippet()
        S.redirectTo("/offers/%d".format(appointment.offer.is))
      }
      case _ => error("no appointment set for edit")
    }
  }

  def processDelete() = {
    appointmentBox match {
      case Full(appointment) => {
        AppointmentAttendee.findAll(By(AppointmentAttendee.appointment, appointment)).map(_.delete_!)
        appointment.delete_!
        S.notice(S ? "appointment.deleted")
        unregisterThisSnippet()
        S.redirectTo("/offers/%d".format(appointment.offer.is))
      }
      case _ => error("no appointment set for delete")
    }
  }
}

class ListAppointment extends DispatchSnippet {
  def dispatch: DispatchIt = _ match {
    case "render" => list
  }

  def list = {

    val isAdmin = User.currentUserIsAdmin.openOr(false)
    val appointments = if (isAdmin) {
      Appointment.findAll()
    } else {
      Appointment.findAll(
        In(Appointment.id, AppointmentAttendee.appointment,
          By(AppointmentAttendee.id, User.currentUserId.openOr(0L))))
    }

    ".appointment-entry" #> appointments.map(a =>
      ".appointment-link [href]" #> {
        if (isAdmin)
          "/appointment/edit/%d".format(a.id.is)
        else
          "/offer/%id".format(a.offer)
      } &
        ".appointment-description" #> a.description &
        ".appointment-spots" #> (if (a.spots <= 0) "--" else ("%d/%d".format(0, a.spots.is))) &
        ".appointment-date" #> {
          if (a.from.getDay == a.till.getDay)
            formatShortDate(a.from)
          else
            "%s - %s".format(formatShortDate(a.from), formatShortDate(a.till))
        } &
        ".appointment-time" #> "%s - %s".format(formatTime(a.from), formatTime(a.till)))
  }
}

class AppointmentPanel extends DispatchSnippet {
  def dispatch: DispatchIt = _ match {
    case "render" => panel
  }

  val isAdmin = User.currentUserIsAdmin.openOr(false)

  def panel = {

    val appointments = S.param("id").map(_.toLong)
      .map(id => Appointment.findAll(By(Appointment.offer, id)))
      .openOr(Nil)

    // TODO: Panel finalisieren (Termine zum Angebot anzeigen und Create Link)
    ".appointment-entry" #> appointments.map(a =>
      ".appointment-description" #> a.description &
        ".appointment-spots" #> {
          if (a.spots <= 0)
            "%d/-".format(a.attendees.size)
          else
            ("%d/%d".format(a.attendees.size, a.spots.is))
        } &
        ".appointment-date" #> {
          if (a.from.getDay == a.till.getDay)
            formatShortDate(a.from)
          else
            "%s - %s".format(formatShortDate(a.from), formatShortDate(a.till))
        } &
        ".appointment-time" #> "%s - %s".format(formatTime(a.from), formatTime(a.till)) &
        ".appointment-deadline" #> (a.deadline match { case null => "keine" case s => formatShortDate(s) }) &
        ".appointment-action" #> buildActionField(a))
  }

  def buildActionField(a: Appointment) = {
    if (!loggedIn_?)
      Text(S ? "you.had.to.be.logged.in")
    else if (isAdmin)
      <a href={ "/appointment/edit/%d".format(a.id.is) }>{ S ? "edit" }</a>
    else if (a.attendees.filter(u => (u.id == User.currentUserId.openOr(-1L))).size > 0)
      SHtml.submit(S ? "sign.out", () => println("LooL")) // ToDo: machen
    else if (!User.currentUser.map(User.isFullUser_?(_)).openOr(false))
      <span	class="lift:Menu.item?linktToSelf=true&name=options;">{S ? "you.must.be.a.full.user"}</span>
    else if (a.attendees.size >= { if (a.spots == 0) Int.MaxValue else a.spots.toInt })
      Text(S ? "appointment.full")
    else
      SHtml.submit(S ? "sign.in", () => println("LooL")) // ToDo: machen
  }
}