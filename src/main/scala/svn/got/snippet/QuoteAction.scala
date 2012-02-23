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

class QuoteAction extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show _
    case "list" => list _
    case "new" => createQuote _
    case "delete" => delete _
    case "edit" => edit _
  }

  /**
   * show Method
   * returns quote
   * @param in
   * @return
   */
  def show(in: NodeSeq): NodeSeq = {

    val id = S.param("id").openOr("0").toInt
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("5").toInt
    val entries = Quotes.count

    var quote: List[Quotes] =
      if (id > 0) {
        val queriedQuote = for {
          foundQuote <- Quotes.find(id) ?~ "Quote not found" ~> 404
        } yield foundQuote
        queriedQuote.toList
      } else {
        for {
          foundQuote <- Quotes.findAll(
            OrderBy(Quotes.id, Descending),
            StartAt((page - 1) * pagesize),
            MaxRows(pagesize))
        } yield foundQuote
      }

    // make prev page link if required
    def prev(in: NodeSeq) =
      if (id > 0 || page <= 1) Text("")
      else <a href={ "/quotes/page/" + (page - 1) }>{ S ? "newer" }</a>

    // make next page link if required
    def next(in: NodeSeq) =
      if (id > 0 || maxpages(entries, pagesize) <= page) Text("")
      else <a href={ "/quotes/page/" + (page + 1) }>{ S ? "older" }</a>

    // processes the given quote entries to real HTML-entries 
    def bindQuote(in: NodeSeq): NodeSeq = quote.flatMap(n => bind("entry", in,
      "title" -> Text(n.title.is),
      "text" -> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)),
      "date" -> Text(timestamp.format(n.createDate.is)),
      "author" -> Text(n.author.getName),
      "id" -> Text(n.id.is.toString)))

    bind("quote", in,
      "entries" -> bindQuote _,
      "previsious" -> prev _,
      "next" -> next _)
  }

  def list(in: NodeSeq): NodeSeq = {
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("50").toInt
    val entries = Quotes.count

    val quote: List[Quotes] =
      for {
        foundQuote <- Quotes.findAll(
          OrderBy(Quotes.id, Descending),
          StartAt((page - 1) * pagesize),
          MaxRows(pagesize))
      } yield foundQuote

    // make prev page link if required
    def prev(in: NodeSeq) =
      if (page <= 1) Text("")
      else <a href={ "/admin/quotes/list/page/" + (page - 1) }>{ S ? "newer" }</a>

    // make next page link if required
    def next(in: NodeSeq) =
      if (maxpages(entries, pagesize) <= page) Text("")
      else <a href={ "/admin/quotes/list/page/" + (page + 1) }>{ S ? "older" }</a>

    // processes the given quote entries to real HTML-entries 
    def bindQuote(in: NodeSeq): NodeSeq = quote.flatMap(n => bind("entry", in,
      "title" -> Text(n.title.is),
      "date" -> Text(timestamp.format(n.createDate.is)),
      "author" -> Text(n.author.getName),
      "id" -> Text(n.id.is.toString),
      "editlink" -> <a href={ "/admin/quotes/edit/" + n.id.is }>Edit</a>,
      "deletelink" -> <a href={ "/admin/quotes/delete/" + n.id.is }>Delete</a>))

    bind("quote", in,
      "entries" -> bindQuote _,
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
   * createQuote Method
   * @param in
   * @return
   */
  def createQuote(in: NodeSeq): NodeSeq = {
    val quote = Quotes.create
    quote.author(curUserId.is.open_!)

    def addQuoteToDatabase() = {
      if (quote.title.is.eq("") && quote.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        quote.save
        S.redirectTo("/quotes")
      }
    }

    bind("quote", in,
      "title" -> SHtml.text(quote.title.toString, quote.title(_)),
      "text" -> SHtml.textarea(quote.text.toString, quote.text(_)),
      "author" -> Text(quote.author.getName),
      "submit" -> SHtml.submit(S ? "add", addQuoteToDatabase))
  }

  def delete(in: NodeSeq): NodeSeq = {
    val id = S.param("id").openOr("0").toInt

    val quote = for {
      foundQuote <- Quotes.find(id) ?~ "Quote not found" ~> 404
    } yield foundQuote

    if (quote.isEmpty)
      S.redirectTo("/")

    val n = quote.open_!

    def deleteQuoteFormDatabase() = {
      n.delete_!
      S.redirectTo("/admin/quotes/list")
    }

    bind("quote", in,
      "title" -> Text(n.title.is),
      "text" -> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)),
      "date" -> Text(timestamp.format(n.createDate.is)),
      "author" -> Text(n.author.getName),
      "id" -> Text(n.id.is.toString),
      "submit" -> SHtml.submit(S ? "delete", deleteQuoteFormDatabase))
  }
  
  def edit(in: NodeSeq): NodeSeq = { 
	val id = S.param("id").openOr("0").toInt

    val queriedQuote = for {
      foundQuote <- Quotes.find(id) ?~ "Quote not found" ~> 404
    } yield foundQuote

    if (queriedQuote.isEmpty)
      S.redirectTo("/")

    val quote = queriedQuote.open_!

    quote.author(curUserId.is.open_!)

    def updateQuoteInDatabase() = {
      if (quote.title.is.eq("") && quote.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        quote.save
        S.redirectTo("/quotes")
      }
    }

    bind("quote", in,
      "title" -> SHtml.text(quote.title.toString, quote.title(_)),
      "text" -> SHtml.textarea(quote.text.toString, quote.text(_)),
      "author" -> Text(quote.author.getName),
      "submit" -> SHtml.submit(S ? "edit", updateQuoteInDatabase))	  
  }
}