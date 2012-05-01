package de.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._

class DateRegistration extends LongKeyedMapper[DateRegistration] with IdPK {
  def getSingleton = DateRegistration
  
  object applicant extends MappedLongForeignKey(this, User)
  
  object date extends MappedLongForeignKey(this, Date)
  
  object registrationDate extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date
  }
  
  object note extends MappedString(this, 512) // For future use
}

object DateRegistration extends DateRegistration with LongKeyedMetaMapper[DateRegistration]