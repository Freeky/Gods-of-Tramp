package svn.got.snippet
import net.liftweb.common.Failure
import net.liftweb.mapper._
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import Helpers._
import svn.got.model._
import S._
import java.text.SimpleDateFormat
import svn.got.main._
import net.liftweb.textile._

class NewsAction extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show _
    case "list" => list _
    case "new" => createNews _
    case "delete" => delete _
    case "edit" => edit _
  }

  /**
   * show Method
   * returns news
   * @param in
   * @return
   */
  def show(in: NodeSeq): NodeSeq = {

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
    def prev(in: NodeSeq) =
      if (id > 0 || page <= 1) Text("")
      else <a href={ "/news/page/" + (page - 1) }>{ S ? "newer" }</a>

    // make next page link if required
    def next(in: NodeSeq) =
      if (id > 0 || maxpages(entries, pagesize) <= page) Text("")
      else <a href={ "/news/page/" + (page + 1) }>{ S ? "older" }</a>

    // processes the given news entries to real HTML-entries 
    def bindNews(in: NodeSeq): NodeSeq = news.flatMap(n => bind("entry", in,
      "title" -> Text(n.title.is),
      "text" -> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)),
      "date" -> Text(timestamp.format(n.createDate.is)),
      "author" -> Text(n.author.getName),
      "id" -> Text(n.id.is.toString)))

    bind("news", in,
      "entries" -> bindNews _,
      "previsious" -> prev _,
      "next" -> next _)
  }

  def list(in: NodeSeq): NodeSeq = {
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
    def prev(in: NodeSeq) =
      if (page <= 1) Text("")
      else <a href={ "/admin/news/list/page/" + (page - 1) }>{ S ? "newer" }</a>

    // make next page link if required
    def next(in: NodeSeq) =
      if (maxpages(entries, pagesize) <= page) Text("")
      else <a href={ "/admin/news/list/page/" + (page + 1) }>{ S ? "older" }</a>

    // processes the given news entries to real HTML-entries 
    def bindNews(in: NodeSeq): NodeSeq = news.flatMap(n => bind("entry", in,
      "title" -> Text(n.title.is),
      "date" -> Text(timestamp.format(n.createDate.is)),
      "author" -> Text(n.author.getName),
      "id" -> Text(n.id.is.toString),
      "editlink" -> <a href={ "/admin/news/edit/" + n.id.is }>Edit</a>,
      "deletelink" -> <a href={ "/admin/news/delete/" + n.id.is }>Delete</a>))

    bind("news", in,
      "entries" -> bindNews _,
      "previsious" -> prev _,
      "next" -> next _)
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
  def createNews(in: NodeSeq): NodeSeq = {
    val news = News.create
    news.author(curUserId.is.open_!)

    def addNewsToDatabase() = {
      if (news.title.is.eq("") && news.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        news.save
        S.redirectTo("/news")
      }
    }

    bind("news", in,
      "title" -> SHtml.text(news.title.toString, news.title(_)),
      "text" -> SHtml.textarea(news.text.toString, news.text(_)),
      "author" -> Text(news.author.toString),
      "submit" -> SHtml.submit(S ? "add", addNewsToDatabase))
  }

  def delete(in: NodeSeq): NodeSeq = {
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

    bind("news", in,
      "title" -> Text(n.title.is),
      "text" -> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)),
      "date" -> Text(timestamp.format(n.createDate.is)),
      "author" -> Text(n.author.getName),
      "id" -> Text(n.id.is.toString),
      "submit" -> SHtml.submit(S ? "delete", deleteNewsFormDatabase))
  }
  
  def edit(in: NodeSeq): NodeSeq = { 
	val id = S.param("id").openOr("0").toInt

    val queriedNews = for {
      foundNews <- News.find(id) ?~ "News not found" ~> 404
    } yield foundNews

    if (queriedNews.isEmpty)
      S.redirectTo("/")

    val news = queriedNews.open_!

    news.author(curUserId.is.open_!)

    def updateNewsInDatabase() = {
      if (news.title.is.eq("") && news.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        news.save
        S.redirectTo("/news")
      }
    }

    bind("news", in,
      "title" -> SHtml.text(news.title.toString, news.title(_)),
      "text" -> SHtml.textarea(news.text.toString, news.text(_)),
      "author" -> Text(news.author.toString),
      "submit" -> SHtml.submit(S ? "edit", updateNewsInDatabase))	  
  }
}