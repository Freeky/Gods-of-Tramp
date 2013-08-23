package de.got.snippet
import net.liftweb.common.Failure
import net.liftweb.mapper._
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import Helpers._
import de.got.model._
import S._
import java.text.SimpleDateFormat
import de.got.main._
import de.got.lib.AjaxFactory._
import net.liftmodules.textile._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds

class EventAction extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show
    case "list" => list
    case "new" => createEvent
    case "delete" => delete
    case "edit" => edit
  }

  /**
   * show Method
   * returns event
   * @param in
   * @return
   */
  def show = {

    val id = S.param("id").openOr("0").toInt
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("5").toInt
    val entries = Event.count

    var event: List[Event] =
      if (id > 0) {
        val queriedEvent = for {
          foundEvent <- Event.find(id) ?~ "Event not found" ~> 404
        } yield foundEvent
        queriedEvent.toList
      } else {
        for {
          foundEvent <- Event.findAll(
            OrderBy(Event.order, Ascending),
            StartAt((page - 1) * pagesize),
            MaxRows(pagesize))
        } yield foundEvent
      }

    // make prev page link if required
    def prev =
      if (id > 0 || page <= 1) ("*" #> "")
      else "* [href]" #> "/events/page/".format(page - 1) &
        "* *" #> (S ? "newer")

    // make next page link if required
    def next =
      if (id > 0 || maxpages(entries, pagesize) <= page) ("*" #> "")
      else "* [href]" #> "/events/page/".format(page + 1) &
        "* *" #> (S ? "older")

    // processes the given event entries to real HTML-entries 
    def bindEvent = event.map(n =>
      ".title" #> Text(n.title.is) &
        ".text" #> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)) &
        ".date" #> Text(timestamp.format(n.createDate.is)) &
        ".author" #> Text(n.author.getName) &
        ".id" #> Text(n.id.is.toString) &
        "title *" #> n.title &
        "@description [content]" #> n.description &
        "@keywords [content]" #> n.keywords.is)

    ".entry" #> bindEvent &
      ".previsious" #> prev &
      ".next" #> next
  }

  def list = {
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("50").toInt
    val entries = Event.count

    val event: List[Event] =
      for {
        foundEvent <- Event.findAll(
          OrderBy(Event.id, Descending),
          StartAt((page - 1) * pagesize),
          MaxRows(pagesize))
      } yield foundEvent

    // make prev page link if required
    def prev =
      if (page <= 1) ("*" #> "")
      else "* [href]" #> "/admin/events/list/page/%d".format(page - 1) &
        "* *" #> (S ? "newer")

    // make next page link if required
    def next =
      if (maxpages(entries, pagesize) <= page) ("*" #> "")
      else "* [href]" #> "/admin/events/list/page/".format(page + 1) &
        "* *" #> (S ? "older")

    // processes the given event entries to real HTML-entries 
    def bindEvent = event.map(n =>
      ".title" #> Text(n.title.is) &
        ".date" #> Text(timestamp.format(n.createDate.is)) &
        ".author" #> Text(n.author.getName) &
        ".id" #> Text(n.id.is.toString) &
        ".order" #> Text(n.order.is.toString) &
        ".editlink [href]" #> "/admin/events/edit/%d".format(n.id.is) &
        ".deletelink [href]" #> "/admin/events/delete/%d".format(n.id.is))

    ".entry" #> bindEvent &
      ".previsious" #> prev &
      ".next" #> next
  }

  /**
   * maxpages Method
   * calculates max amount of pages for given entries,
   * why there could be max $pagesize entries per page
   * @param entries
   * @param pagesize
   * @return
   */
  def maxpages(entries: Long, pagesize: Int): Long =
    if (entries % pagesize > 0)
      (entries / pagesize) + 1
    else
      entries / pagesize

  def timestamp = new SimpleDateFormat("dd.MM.yyyy")

  /**
   * createEvent Method
   * @param in
   * @return
   */
  def createEvent = {
    val event = Event.create
    event.author(curUser.map(_.id.is).openOr(0L))

    def addEventToDatabase() = {
      if (event.title.is.eq("") && event.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        event.save
        S.redirectTo("/events")
      }
    }

    def preview = {
      TextileParser.toHtml(event.text.toString)
    }

    def updatePreview(text: String): JsCmd = {
      event.text(text)
      JsCmds.SetHtml("previewarea", preview)
    }

    ".title" #> SHtml.text(event.title.toString, event.title(_)) &
      ".text" #> ajaxLiveTextarea(event.text.toString, updatePreview) &
      ".description" #> SHtml.text(event.description, event.description(_)) &
      ".keywords" #> SHtml.text(event.keywords, event.keywords(_)) &
      ".order" #> SHtml.text(event.order.toString, x => event.order(x.toInt)) &
      ".author" #> Text(event.author.getName) &
      ".submit" #> SHtml.submit(S ? "add", addEventToDatabase) &
      "#previewarea *" #> preview
  }

  def delete = {
    val id = S.param("id").openOr("0").toInt

    val event = for {
      foundEvent <- Event.find(id) ?~ "Event not found" ~> 404
    } yield foundEvent

    if (event.isEmpty)
      S.redirectTo("/")

    val n = event.open_!

    def deleteEventFormDatabase() = {
      n.delete_!
      S.redirectTo("/admin/events/list")
    }

    ".title" #> Text(n.title.is) &
      ".text" #> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)) &
      ".description" #> Text(n.description.is) &
      ".keywords" #> SHtml.text(n.keywords, n.keywords(_)) &
      ".date" #> Text(timestamp.format(n.createDate.is)) &
      ".author" #> Text(n.author.getName) &
      ".id" #> Text(n.id.is.toString) &
      ".submit" #> SHtml.submit(S ? "delete", deleteEventFormDatabase)
  }

  def edit = {
    val id = S.param("id").openOr("0").toInt

    val queriedEvent = for {
      foundEvent <- Event.find(id) ?~ "Event not found" ~> 404
    } yield foundEvent

    if (queriedEvent.isEmpty)
      S.redirectTo("/")

    val event = queriedEvent.open_!

    event.author(curUser.map(_.id.is).openOr(0L))

    def updateEventInDatabase() = {
      if (event.title.is.eq("") && event.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        event.save
        S.redirectTo("/events")
      }
    }
    
    def preview = {
      TextileParser.toHtml(event.text.toString)
    }

    def updatePreview(text: String): JsCmd = {
      event.text(text)
      JsCmds.SetHtml("previewarea", preview)
    }

    ".title" #> SHtml.text(event.title.toString, event.title(_)) &
      ".text" #> ajaxLiveTextarea(event.text.toString, updatePreview) &
      ".description" #> SHtml.text(event.description, event.description(_)) &
      ".keywords" #> SHtml.text(event.keywords, event.keywords(_)) &
      ".order" #> SHtml.text(event.order.toString, x => event.order(x.toInt)) &
      ".author" #> Text(event.author.getName) &
      ".submit" #> SHtml.submit(S ? "edit", updateEventInDatabase) &
      "#previewarea *" #> preview
  }
}