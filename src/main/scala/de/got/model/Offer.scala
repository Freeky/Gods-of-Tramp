package de.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._

class Offer extends LongKeyedMapper[Offer] with IdPK {
  def getSingleton = Offer

  object title extends MappedString(this, 150)
  object createDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object editDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object text extends MappedText(this)
  
  object description extends MappedString(this, 300)

  object author extends MappedLongForeignKey(this, User) {
    def getName: String = {
      val user = User.find(this.is)
      if (user.isDefined) {
        user.open_!.name.is
      } else {
        "noname"
      }
    }
  }

  object order extends MappedInt(this) {
    override def defaultValue = 9999
  }
}

object Offer extends Offer with LongKeyedMetaMapper[Offer]