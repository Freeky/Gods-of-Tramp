package svn.got.snippet
import net.liftweb.mapper._
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import Helpers._
import svn.got.model._
import S._
import net.liftweb.textile._

class StaticPage extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "show" => show
    case "list" => list
    case "generate" => generate
    case "edit" => edit
  }

  def show(in: NodeSeq): NodeSeq = {
    val site = S.attr("site") openOr ""
    val contentBox = for {
      staticSite <- StaticPage.find(By(StaticPage.name, site))
    } yield staticSite.content.is

    contentBox match {
      case Full(content) => Text(content)
//        TextileParser.paraFixer(
//          TextileParser.toHtml(content))
      case _ => {
    	  generatePages
    	  S.redirectTo("/")
      }
    }
  }

  def list(in: NodeSeq): NodeSeq = {
    StaticPage.findAll.flatMap(page => bind("page", in,
      "id" -> Text(page.id.is.toString),
      "name" -> Text(page.name.is),
      "editlink" -> <a href={ "/admin/staticpage/edit/" + page.id.is }>Edit</a>))
  }

  def edit(in: NodeSeq): NodeSeq = {
	  val pageId = (S.param("id") openOr "0").toInt
	  val dbPage = StaticPage.find(pageId)
	  
	  def processEdit() = {
	 	  dbPage.open_!.save
	  }
	   
	  dbPage match {
	 	  case Full(page) => bind("page", in, 
	 		  "name" -> Text(page.name.is),
	 		  "editcontent" -> SHtml.textarea(page.content.is, page.content(_)),
	 		  "showcontent" -> TextileParser.paraFixer(TextileParser.toHtml(page.content.is)),
	 		  "submit" -> SHtml.submit(S ? "edit", processEdit))
	 	  case _ => S.redirectTo("/admin/staticpage/list")
	  }
	  
  }
  def generate(in: NodeSeq): NodeSeq = {
    bind("form", in,
      "submit" -> SHtml.submit(S ? "generate.static.pages", generatePages))
  }
  
  def generatePages() = {
    	if(StaticPage.find(By(StaticPage.name, "home")) == Empty)
    	StaticPage.create.name("home").content("").save
    	if(StaticPage.find(By(StaticPage.name, "aboutus")) == Empty)
    	StaticPage.create.name("aboutus").content("").save
    	if(StaticPage.find(By(StaticPage.name, "links")) == Empty)
    	StaticPage.create.name("links").content("").save
    	if(StaticPage.find(By(StaticPage.name, "contact")) == Empty)
    	StaticPage.create.name("contact").content("").save
    }
}