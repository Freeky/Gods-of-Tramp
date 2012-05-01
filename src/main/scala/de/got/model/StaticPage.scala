package de.got.model
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import Helpers._
import net.liftweb.mapper._

class StaticPage extends LongKeyedMapper[StaticPage] with IdPK {
  def getSingleton = StaticPage

  object name extends MappedString(this, 128){
	  override def dbIndexed_? = true
  }

  object content extends MappedText(this){
	  override def dbNotNull_? = true
  }
}

object StaticPage extends StaticPage with LongKeyedMetaMapper[StaticPage]