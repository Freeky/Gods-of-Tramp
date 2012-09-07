package de.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._

class Appointment extends LongKeyedMapper[Appointment] with IdPK with ManyToMany{
  def getSingleton = Appointment

  object description extends MappedString(this, 512)
  object createDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  object editDate extends MappedDate(this) {
    override def defaultValue = new java.util.Date
  }
  
  object owner extends MappedLongForeignKey(this, User) {
    def getName: String = {
      User.find(this.is).map(_.name.is).openOr("noname")
    }
  }
  object from extends MappedDateTime(this) {
    override def defaultValue = time(millis + days(1))
  }
  object till extends MappedDateTime(this) {
    override def defaultValue = time(millis + days(1))
  }
  
  object spots extends MappedInt(this)
  
  object offer extends MappedLongForeignKey(this, Offer)
  
  object deadline extends MappedDateTime(this)
  
  object attendees extends MappedManyToMany(AppointmentAttendee, 
      AppointmentAttendee.appointment, AppointmentAttendee.attendee, User)
}

object Appointment extends Appointment with LongKeyedMetaMapper[Appointment]