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
import net.liftweb.textile._

class OfferAction extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show
    case "list" => list
    case "new" => createOffer
    case "delete" => delete
    case "edit" => edit
  }

  /**
   * show Method
   * returns offer
   * @param in
   * @return
   */
  def show = {

    val id = S.param("id").openOr("0").toInt
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("5").toInt
    val entries = Offer.count

    var offer: List[Offer] =
      if (id > 0) {
        val queriedOffer = for {
          foundOffer <- Offer.find(id) ?~ "Offer not found" ~> 404
        } yield foundOffer
        queriedOffer.toList
      } else {
        for {
          foundOffer <- Offer.findAll(
            OrderBy(Offer.order, Ascending),
            StartAt((page - 1) * pagesize),
            MaxRows(pagesize))
        } yield foundOffer
      }

    // make prev page link if required
    def prev =
      if (id > 0 || page <= 1) ("*" #> "")
      else "* [href]" #> "/offers/page/".format(page - 1) &
        "* *" #> (S ? "newer")

    // make next page link if required
    def next =
      if (id > 0 || maxpages(entries, pagesize) <= page) ("*" #> "")
      else "* [href]" #> "/offers/page/".format(page + 1) &
        "* *" #> (S ? "older")

    // processes the given offer entries to real HTML-entries 
    def bindOffer = offer.map(n =>
      ".title" #> Text(n.title.is) &
        ".text" #> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)) &
        ".date" #> Text(timestamp.format(n.createDate.is)) &
        ".author" #> Text(n.author.getName) &
        ".id" #> Text(n.id.is.toString))

    ".entry" #> bindOffer &
      ".previsious" #> prev &
      ".next" #> next
  }

  def list = {
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("50").toInt
    val entries = Offer.count

    val offer: List[Offer] =
      for {
        foundOffer <- Offer.findAll(
          OrderBy(Offer.id, Descending),
          StartAt((page - 1) * pagesize),
          MaxRows(pagesize))
      } yield foundOffer

    // make prev page link if required
    def prev =
      if (page <= 1) ("*" #> "")
      else "* [href]" #> "/admin/offers/list/page/%d".format(page - 1) &
        "* *" #> (S ? "newer")

    // make next page link if required
    def next =
      if (maxpages(entries, pagesize) <= page) ("*" #> "")
      else "* [href]" #> "/admin/offers/list/page/".format(page + 1) &
        "* *" #> (S ? "older")

    // processes the given offer entries to real HTML-entries 
    def bindOffer = offer.map(n =>
      ".title" #> Text(n.title.is) &
        ".date" #> Text(timestamp.format(n.createDate.is)) &
        ".author" #> Text(n.author.getName) &
        ".id" #> Text(n.id.is.toString) &
        ".order" #> Text(n.order.is.toString) &
        ".editlink [href]" #> "/admin/offers/edit/%d".format(n.id.is) &
        ".deletelink [href]" #> "/admin/offers/delete/%d".format(n.id.is))

    ".entry" #> bindOffer &
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
   * createOffer Method
   * @param in
   * @return
   */
  def createOffer = {
    val offer = Offer.create
    offer.author(curUserId.is.open_!)

    def addOfferToDatabase() = {
      if (offer.title.is.eq("") && offer.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        offer.save
        S.redirectTo("/offers")
      }
    }

    ".title" #> SHtml.text(offer.title.toString, offer.title(_)) &
      ".text" #> SHtml.textarea(offer.text.toString, offer.text(_)) &
      ".order" #> SHtml.text(offer.order.toString, x => offer.order(x.toInt)) &
      ".author" #> Text(offer.author.getName) &
      ".submit" #> SHtml.submit(S ? "add", addOfferToDatabase)
  }

  def delete = {
    val id = S.param("id").openOr("0").toInt

    val offer = for {
      foundOffer <- Offer.find(id) ?~ "Offer not found" ~> 404
    } yield foundOffer

    if (offer.isEmpty)
      S.redirectTo("/")

    val n = offer.open_!

    def deleteOfferFormDatabase() = {
      n.delete_!
      S.redirectTo("/admin/offers/list")
    }

    ".title" #> Text(n.title.is) &
      ".text" #> TextileParser.paraFixer(TextileParser.toHtml(n.text.is)) &
      ".date" #> Text(timestamp.format(n.createDate.is)) &
      ".author" #> Text(n.author.getName) &
      ".id" #> Text(n.id.is.toString) &
      ".submit" #> SHtml.submit(S ? "delete", deleteOfferFormDatabase)
  }

  def edit = {
    val id = S.param("id").openOr("0").toInt

    val queriedOffer = for {
      foundOffer <- Offer.find(id) ?~ "Offer not found" ~> 404
    } yield foundOffer

    if (queriedOffer.isEmpty)
      S.redirectTo("/")

    val offer = queriedOffer.open_!

    offer.author(curUserId.is.open_!)

    def updateOfferInDatabase() = {
      if (offer.title.is.eq("") && offer.text.is.eq(""))
        S.error(S.?("fill.title.and.text"))
      else {
        offer.save
        S.redirectTo("/offers")
      }
    }

    ".title" #> SHtml.text(offer.title.toString, offer.title(_)) &
      ".text" #> SHtml.textarea(offer.text.toString, offer.text(_)) &
      ".order" #> SHtml.text(offer.order.toString, x => offer.order(x.toInt)) &
      ".author" #> Text(offer.author.getName) &
      ".submit" #> SHtml.submit(S ? "edit", updateOfferInDatabase)
  }
}