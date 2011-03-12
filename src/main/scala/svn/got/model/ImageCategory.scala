package svn.got.model
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import Helpers._
import net.liftweb.mapper._
import scala.xml.Text
import S._

class ImageCategory extends LongKeyedMapper[ImageCategory] with IdPK {
	def getSingleton = ImageCategory
	
	object name extends MappedString(this, 200) {
		override def defaultValue = (new java.util.Date).toString
	}
	
	object parent extends MappedLongForeignKey(this, ImageCategory)
}

object ImageCategory extends ImageCategory with LongKeyedMetaMapper[ImageCategory]{
	override def afterSchemifier = {
       if(ImageCategory.find(By(ImageCategory.name, "")).isEmpty)
    	   ImageCategory.create.name("").save
	}
}