package de.got.lib

import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.common._
import de.got.model._
import scala.xml.Text

class EventLoc(
  override val name: String,
  override val link: Link[Unit],
  override val text: LinkText[Unit],
  override val params: List[LocParam[Unit]]) extends Loc[Unit] {

  override val defaultValue: Box[Unit] = Full(())
  
  init()
  
  override def supplimentalKidMenuItems: List[MenuItem] = {
    {for {
      event <- Event.findAll()
    } yield MenuItem(
        Text(event.title.is),
        Text("/events/%d".format(event.id.is)), Nil, false, false,
        allParams.flatMap {
          case v: Loc.LocInfo[_] => List(v())
          case _ => Nil
        })
    } ::: super.supplimentalKidMenuItems
  }
}