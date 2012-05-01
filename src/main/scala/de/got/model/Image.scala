package de.got.model
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import scala.xml.Node
class Image extends LongKeyedMapper[Image] with IdPK with ManyToMany {
  def getSingleton = Image

  object name extends MappedString(this, 128)

  object secure extends MappedUniqueId(this, 10)

  object mimeType extends MappedString(this, 64)
  object blob extends MappedBinary(this)

  object uploadDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  
  object uploader extends MappedLongForeignKey(this, User) {
}

object Image extends Image with LongKeyedMetaMapper[Image] {
	def toHTML(img: Image): Node = 
		<img src={"/image/" + img.secure.is + "/" + img.name.is}>{img.name.is}</img>;
}