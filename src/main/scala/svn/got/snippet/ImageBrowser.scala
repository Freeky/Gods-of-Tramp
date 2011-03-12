package svn.got.snippet
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._
import svn.got.model._
import S._

class ImageBrowser extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "categories" => categories
    case "images" => images
  }

  def categories(in: NodeSeq): NodeSeq = Text("")

  def images(in: NodeSeq): NodeSeq = {

    val columns = S.attr("columns").openOr("2").toInt
    val imageWidth = S.attr("width").openOr("300").toInt
    val imageHeight = S.attr("height").openOr("300").toInt
    val categoryName = S.param("category").openOr("")

    val categoryId = ImageCategory.find(By(ImageCategory.name, categoryName))
    val images: List[Image] =
      for {
        mappedImage <- ImageToCategory.findAll(
          By(ImageToCategory.category, categoryId))
        img <- mappedImage.image.foreign
      } yield img
      
   
   def bindImageEntries(in: NodeSeq): NodeSeq = 
	   images.flatMap(img => bind(in, "entry",
	  		   "content" -> ))
   bind("image", in, 
		   "entry" -> )

    Text("")
  }
}