package svn.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._

class News extends LongKeyedMapper[News] with IdPK {
  def getSingleton = News
  object title extends MappedString(this, 150)
  object createDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object editDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object text extends MappedText(this)
  object author extends MappedString(this, 40)
  
  //ToDo:
  //object author extends MappedLongForeignKey(this, User)
}

object News extends News with LongKeyedMetaMapper[News]