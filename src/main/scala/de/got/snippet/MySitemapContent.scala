package de.got.snippet

import net.liftweb.common._
import net.liftweb.mapper._
import scala.xml.{ NodeSeq, Text }
import net.liftweb.util.Helpers
import net.liftweb.http._
import net.liftweb.proto.ProtoRules
import Helpers._
import de.got.model._
import S._
import de.got.main._
import net.liftweb.util._
import java.text.SimpleDateFormat



class MySitemapContent {

  case class Entry(date: java.util.Date, url: String)
  
  lazy val lastModFormat = new SimpleDateFormat()
  lastModFormat.applyPattern("yyyy-MM-dd")
  
  lazy val entries: List[Entry] = news :: pictures :: static ::: Nil
  lazy val static: List[Entry] = StaticPage.findAll().map(page => Entry(page.lastModified.is, "/%s".format(page.name.is)))
  lazy val news = Entry(News.findAll(OrderBy(News.editDate, Descending)).head.editDate.is, "/news")
  lazy val pictures = Entry(Image.findAll(OrderBy(Image.uploadDate, Descending)).head.uploadDate.is, "/pictures")
  
  def base: CssSel =
    "loc *" #> "http://%s/".format(S.hostName) &
    "lastmod *" #> lastModFormat.format(Helpers.now)

  def list: CssSel =
    "url *" #> entries.map(post =>
      "loc *" #> "http://%s%s".format(S.hostName, post.url) &
      "lastmod *" #>  lastModFormat.format(post.date))

}