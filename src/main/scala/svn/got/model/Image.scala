package svn.got.model
import scala.xml.Node

import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import Helpers._
import net.liftweb.mapper._
import S._

class Image extends LongKeyedMapper[Image] with IdPK {
  def getSingleton = Image

  object name extends MappedString(this, 128)

  object secure extends MappedUniqueId(this, 10)

  object mimeType extends MappedString(this, 64)
  object blob extends MappedBinary(this)

  object uploadDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  
  object uploader extends MappedLongForeignKey(this, User) {	def getName: String = {      val user = User.find(this.is);      if (user.isDefined) {        user.open_!.name.is      } else {        "noname"      }    }  }
}

object Image extends Image with LongKeyedMetaMapper[Image] {	def toHTML(img: Image, width: Integer, height: Integer): Node =		<img src={String.format("/image/%s/%s?width=%d&height=%d", img.secure.is, img.name.is, width, height)}>{img.name.is}</img>		
	def toHTML(img: Image): Node = 
		<img src={"/image/" + img.secure.is + "/" + img.name.is}>{img.name.is}</img>;		def detailLink(img: Image, text: String): Node =		<a href={ "/admin/picture/detail/" + img.id.is }>{text}</a>;			def deleteLink(img: Image, text: String): Node =		<a href={ "/admin/picture/delete/" + img.id.is }>{text}</a>;		
}