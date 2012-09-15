package de.got.model
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import scala.xml.Node
import java.io.File
class Image extends LongKeyedMapper[Image] with IdPK with ManyToMany {
  def getSingleton = Image

  object name extends MappedString(this, 128)

  object secure extends MappedUniqueId(this, 10)

  object mimeType extends MappedString(this, 64)

  object uploadDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  
  object uploader extends MappedLongForeignKey(this, User) {	def getName: String = {      val user = User.find(this.is);      if (user.isDefined) {        user.open_!.name.is      } else {        "noname"      }    }  };    object categories extends MappedManyToMany(ImageToCategory,       ImageToCategory.image, ImageToCategory.category, ImageCategory)
}

object Image extends Image with LongKeyedMetaMapper[Image] {	def toHTML(img: Image, width: Int, height: Int): Node = {		<img src={ "/image/%s/%s?width=%d&height=%d".format(img.secure.is, img.name.is, width, height)}>{img.name.is}</img>	};		
	def toHTML(img: Image): Node = {
		<img src={"/image/" + img.secure.is + "/" + img.name.is}>{img.name.is}</img>	};		def detailLink(img: Image): String = {		"/admin/picture/detail/%d" format img.id.is	};		def deleteLink(img: Image): String = {		"/admin/picture/delete/%d" format img.id.is	};		def file(img: Image) = {		val path = new File(Props.get("imagepath", "./images"));		val file = new File(path, img.id.toString);		file	};		
}