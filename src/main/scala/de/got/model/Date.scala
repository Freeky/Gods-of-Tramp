package de.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._

class Date extends LongKeyedMapper[Date] with IdPK with ManyToMany{
  def getSingleton = Date

  object name extends MappedString(this, 128)
  object createDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object editDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object description extends MappedText(this)

  object owner extends MappedLongForeignKey(this, User) {
    def getName: String = {
      val user = User.find(this.is)
      if (user.isDefined) {
        user.open_!.name.is
      } else {
        "noname"
      }
    }
  }
  object date extends MappedDateTime(this) {
    override def defaultValue = time(millis + days(1))
  }
  
  object spots extends MappedInt(this)
  
  object registrations extends MappedManyToMany(DateRegistration, 
      DateRegistration.date, DateRegistration.applicant, User)
}

object Date extends Date with LongKeyedMetaMapper[Date]