package de.got.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import net.liftweb.mapper._
import de.got.main._

class AccountType extends LongKeyedMapper[AccountType] with IdPK {
  def getSingleton = AccountType

  object name extends MappedString(this, 20)
  object description extends MappedString(this, 1000)
  object isAdmin extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object isEnabled extends MappedBoolean(this) {
    override def defaultValue = true
  }
}

object AccountType extends AccountType with LongKeyedMetaMapper[AccountType] {
	override def afterSchemifier(): Unit = {
	  this.findOrCreate(1).name("User").description("Standard user").save()
	  this.findOrCreate(2).name("Administrator").description("Allowed to do everything").isAdmin(true).save
	  this.findOrCreate(3).name("Disabled").description("This account is disabled").isEnabled(false).save
	}
}