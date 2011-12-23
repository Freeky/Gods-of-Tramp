package svn.got.model
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import svn.got.main._

class User extends LongKeyedMapper[User] with IdPK {
	def getSingleton = User
	
	object name extends MappedString(this, 20)
	object password extends MappedPassword(this)
	object email extends MappedEmail(this,256)
	object registrationDate extends MappedDate(this){
		override def defaultValue = new java.util.Date
	}
	object isAdmin extends MappedBoolean(this){
		override def defaultValue = false
	}
	object isEnabled extends MappedBoolean(this){
		override def defaultValue = true
	}
}

object User extends User with LongKeyedMetaMapper[User] {
	def currentUserId: Box[Long] = curUserId.is
	def loggedIn_?() = currentUserId.isDefined
	def currentUserIsAdmin: Box[Boolean] = curUserIsAdmin.is
	def isAdmin_?() = currentUserIsAdmin.isDefined
}