package de.got.model
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import de.got.main._
import org.joda.time._

class User extends LongKeyedMapper[User] with IdPK {
	def getSingleton = User
	
	object name extends MappedString(this, 20) {
	  override def dbIndexed_? = true;
	}
	object password extends MappedPassword(this)
	object email extends MappedEmail(this,256) {
	  override def dbIndexed_? = true;
	}
	object registrationDate extends MappedDate(this){
		override def defaultValue = new java.util.Date
	}
	object accountType extends MappedLongForeignKey(this, AccountType) {
	  override def defaultValue = 1L
	}
	object wantsNewsletter extends MappedBoolean(this){
	  override def defaultValue = true
	}
	object firstName extends MappedString(this, 40)
	object lastName extends MappedString(this, 40)
	object birthday extends MappedDate(this)
	object parentFirstName extends MappedString(this, 40)
	object parentLastName extends MappedString(this, 40)
	object street extends MappedString(this, 50)
	object postalCode extends MappedString(this, 5)
	object city extends MappedString(this, 20)
	object phoneNumber extends MappedString(this, 20)
}

object User extends User with LongKeyedMetaMapper[User] {
	def currentUserId: Box[Long] = curUserId.is
	def loggedIn_?() = currentUserId.isDefined
	def currentUserIsAdmin: Box[Boolean] = curUserIsAdmin.is
	def isAdmin_?() = currentUserIsAdmin.isDefined
	
	def isFullUser_?(u: User) = {
	  var is = true;
	  
	  if(u.firstName.length() <= 0) is = false;
	  if(u.lastName.length() <= 0) is = false;
	  if(u.birthday.is == null) is = false;
	  println("")
	  if(u.street.length() <=0 ) is = false;
	  if(u.postalCode.length() != 5) is = false;
	  if(u.city.length() <= 0) is = false;
	  if(u.phoneNumber.length() <= 0) is = false;
	  
	  val age = Years.yearsBetween(new DateTime(u.birthday.is), new DateTime(Helpers.now)).getYears()
	  if(age < 18 && u.parentFirstName.length() <= 0) is = false;
	  if(age < 18 && u.parentLastName.length() <= 0) is = false;
	  
	  is
	}
	
}