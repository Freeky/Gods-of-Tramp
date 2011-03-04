package svn.got.snippet
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._
import svn.got.model._
import S._

class ImageBrowser extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "categories" => categories
    case "images" => images
  }

  def categories(in: NodeSeq): NodeSeq = Text("")

  def images(in: NodeSeq): NodeSeq = Text("")
}