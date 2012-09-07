package de.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._

object AttendeeStatus extends Enumeration {
  type Enumeration = Value
  val Attend = Value(1, "attend")
  val Waitlist = Value(2, "waitlist")
  val Unconfirmed = Value(3, "unconfirmed")
}


class AppointmentAttendee extends LongKeyedMapper[AppointmentAttendee] with IdPK {
  def getSingleton = AppointmentAttendee
  
  object attendee extends MappedLongForeignKey(this, User)
  
  object appointment extends MappedLongForeignKey(this, Appointment)
  
  object registrationDate extends MappedDateTime(this) {
	  override def defaultValue = new java.util.Date
  }
  
  object updateDate extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date
  }
  
  object status extends MappedEnum(this, AttendeeStatus)
  
  object note extends MappedString(this, 512) // For future use
  
  def attendeeName: String = User.find(attendee).map[String](_.name).openOr("unknown")
}

object AppointmentAttendee extends AppointmentAttendee with LongKeyedMetaMapper[AppointmentAttendee]