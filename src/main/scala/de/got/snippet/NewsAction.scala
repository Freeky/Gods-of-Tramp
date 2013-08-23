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

class NewsAction extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show
    case "list" => list
    case "new" => createNews
    case "delete" => delete
    case "edit" => edit
  }

  /**
   * show Method
   * returns news
   * @param in
   * @return
   */
  def show = {

    val id = S.param("id").openOr("0").toInt
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("5").toInt
    val entries = News.count

    var news: List[News] =
      if (id > 0) {
        val queriedNews = for {
          foundNews <- News.find(id) ?~ "News not found" ~> 404
        } yield foundNews
        queriedNews.toList
      } else {
        for {
          foundNews <- News.findAll(
            OrderBy(News.id, Descending),
            StartAt((page - 1) * pagesize),
            MaxRows(pagesize))
        } yield foundNews
      }

    // make prev page link if required
    def prev =
      if (id > 0 || page <= 1)
        ("*" #> "")
      else
        "* [href]" #> ("/news/page/%d" format (page - 1)) &
          "* *" #> (S ? "newer")

    // make next page link if required
    def next =
      if (id > 0 || maxpages(entries, pagesize) <= page)
        ("*" #> "")
      else
        "* [href]" #> ("/news/page/%d" format (page + 1)) &
          "* *" #> (S ? "older")

    // processes the given news entries to real HTML-entries 
    def bindNews = news.map(n =>
      ".title *" #> n.title.is &
        ".text *" #> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)) &
        ".date" #> timestamp.format(n.createDate.is) &
        ".author" #> n.author.getName &
        ".id" #> n.id.is.toString)

    ".entry" #> bindNews &
      ".previsious" #> prev &
      ".next" #> next
  }

  def list = {
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("50").toInt
    val entries = News.count

    val news: List[News] =
      for {
        foundNews <- News.findAll(
          OrderBy(News.id, Descending),
          StartAt((page - 1) * pagesize),
          MaxRows(pagesize))
      } yield foundNews

    // make prev page link if required
    def prev =
      if (page <= 1) ("*" #> "")
      else "* [href]" #> "/admin/news/list/page/%d".format(page - 1)

    // make next page link if required
    def next =
      if (maxpages(entries, pagesize) <= page) ("*" #> "")
      else "* [href]" #> "/admin/news/list/page/%d".format(page + 1)

    // processes the given news entries to real HTML-entries 
    def bindNews = news.map(n =>
      ".title" #> Text(n.title.is) &
        ".date" #> Text(timestamp.format(n.createDate.is)) &
        ".author" #> Text(n.author.getName) &
        ".id" #> Text(n.id.is.toString) &
        ".editlink [href]" #> "/admin/news/edit/%d".format(n.id.is) &
        ".deletelink [href]" #> "/admin/news/delete/%d".format(n.id.is))

    ".entry" #> bindNews &
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
   * createNews Method
   * @param in
   * @return
   */
  def createNews = {
    val news = News.create
    news.author(curUser.map(_.id.is).openOr(0L))

    def addNewsToDatabase() = {
      if (news.title.is.eq("") && news.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        news.save
        S.redirectTo("/news")
      }
    }
    
    def preview = TextileParser.toHtml(news.text.toString)

    def updatePreview(text: String): JsCmd = {
      news.text(text)
      JsCmds.SetHtml("previewarea", preview)
    }

    ".title" #> SHtml.text(news.title.toString, news.title(_)) &
      ".text" #> ajaxLiveTextarea(news.text.toString, updatePreview) &
      ".author" #> Text(news.author.getName) &
      ".submit" #> SHtml.submit(S ? "add", addNewsToDatabase) &
      "#previewarea *" #> preview
  }

  def delete = {
    val id = S.param("id").openOr("0").toInt

    val news = for {
      foundNews <- News.find(id) ?~ "News not found" ~> 404
    } yield foundNews

    if (news.isEmpty)
      S.redirectTo("/")

    val n = news.open_!

    def deleteNewsFormDatabase() = {
      n.delete_!
      S.redirectTo("/admin/news/list")
    }

    ".title" #> Text(n.title.is) &
      ".text" #> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)) &
      ".date" #> Text(timestamp.format(n.createDate.is)) &
      ".author" #> Text(n.author.getName) &
      ".id" #> Text(n.id.is.toString) &
      ".submit" #> SHtml.submit(S ? "delete", deleteNewsFormDatabase)
  }

  def edit = {
    val id = S.param("id").openOr("0").toInt

    val queriedNews = for {
      foundNews <- News.find(id) ?~ "News not found" ~> 404
    } yield foundNews

    if (queriedNews.isEmpty)
      S.redirectTo("/")

    val news = queriedNews.open_!

    news.author(curUser.map(_.id.is).openOr(0L))

    def updateNewsInDatabase() = {
      if (news.title.is.eq("") && news.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        news.save
        S.redirectTo("/news")
      }
    }
    
    def preview = TextileParser.toHtml(news.text.toString)

    def updatePreview(text: String): JsCmd = {
      news.text(text)
      JsCmds.SetHtml("previewarea", preview)
    }

    ".title" #> SHtml.text(news.title.toString, news.title(_)) &
      ".text" #> ajaxLiveTextarea(news.text.toString, updatePreview) &
      ".author" #> Text(news.author.getName) &
      ".submit" #> SHtml.submit(S ? "edit", updateNewsInDatabase) &
      "#previewarea *" #> preview
  }
}