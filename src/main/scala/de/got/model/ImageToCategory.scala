package de.got.model
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import Helpers._
import net.liftweb.mapper._
import scala.xml.Text
import S._

class ImageToCategory extends LongKeyedMapper[ImageToCategory] with IdPK {
  def getSingleton = ImageToCategory
  
  object image extends MappedLongForeignKey(this, Image)
  object category extends MappedLongForeignKey(this, ImageCategory)
}

object ImageToCategory extends ImageToCategory with LongKeyedMetaMapper[ImageToCategory]