package de.got.snippet
import net.liftweb.mapper._
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import Helpers._
import de.got.model._
import S._
import net.liftweb.textile._
import net.liftweb.http.js.JsCmds

class StaticPage extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show
    case "list" => list
    case "generate" => generate
    case "edit" => edit
  }

  def show = {
    val site = S.attr("site") openOr ""
    val contentBox = for {
      staticSite <- StaticPage.find(By(StaticPage.name, site))
    } yield staticSite.content.is

    contentBox match {
      case Full(content) => 
        "*" #> TextileParser.paraFixer(
          TextileParser.toHtml(content))
      case _ => {
    	  generatePages
    	  S.redirectTo("/")
      }
    }
  }

  def list = {
    "*" #> StaticPage.findAll.map(page => 
      ".id" #> Text(page.id.is.toString) &
      ".name" #> Text(page.name.is) &
      ".editlink [href]" #> "/admin/staticpage/edit/%d".format(page.id.is))
  }

  def edit = {
	  val pageId = (S.param("id") openOr "0").toInt
	  val dbPage = StaticPage.find(pageId)
	  
	  def processEdit() = {
	 	  dbPage.map(_.lastModified(now).save())
	  }
	  
	  def updateShowContent(text: String) = {
	    dbPage match {
	 	  case Full(page) => 
	 		  page.content(text)
	 		  JsCmds.SetHtml("showcontent", TextileParser.toHtml(text))
	 	  case _ => S.redirectTo("/admin/staticpage/list")
	  }
	  }
	   
	  dbPage match {
	 	  case Full(page) => 
	 		  ".name" #> Text(page.name.is) &
	 		  ".editcontent" #> SHtml.ajaxTextarea(page.content.is, updateShowContent _) &
	 		  "#showcontent *" #> TextileParser.paraFixer(TextileParser.toHtml(page.content.is)) &
	 		  ".submit" #> SHtml.submit(S ? "edit", processEdit)
	 	  case _ => S.redirectTo("/admin/staticpage/list")
	  }
  }
  
  def generate = {
      ".submit" #> SHtml.submit(S ? "generate.static.pages", generatePages)
  }
  
  def generatePages() = {
    	if(StaticPage.find(By(StaticPage.name, "index")) == Empty)
    	StaticPage.create.name("index").content("").save
    	if(StaticPage.find(By(StaticPage.name, "aboutus")) == Empty)
    	StaticPage.create.name("aboutus").content("").save
    	if(StaticPage.find(By(StaticPage.name, "links")) == Empty)
    	StaticPage.create.name("links").content("").save
    	if(StaticPage.find(By(StaticPage.name, "contact")) == Empty)
    	StaticPage.create.name("contact").content("").save
    	if(StaticPage.find(By(StaticPage.name, "agb")) == Empty)
    	StaticPage.create.name("agb").content("").save
    	if(StaticPage.find(By(StaticPage.name, "impressum")) == Empty)
    	StaticPage.create.name("impressum").content("").save
    }
}