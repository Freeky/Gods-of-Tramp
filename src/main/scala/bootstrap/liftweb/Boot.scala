package bootstrap.liftweb

import net.liftweb.mapper.StandardDBVendor
import _root_.net.liftweb.common.Full
import _root_.net.liftweb.mapper.DefaultConnectionIdentifier
import _root_.net.liftweb.mapper.DB
import _root_.net.liftweb.mapper.Schemifier
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

import svn.got.model._
import svn.got.snippet._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    // Database things
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
          "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    //DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    Schemifier.schemify(true, Schemifier.infoF _, News, User, Image, StaticPage, ImageCategory, ImageToCategory)
    S.addAround(DB.buildLoanWrapper)
    
    // where to search snippet
    LiftRules.addToPackages("svn.got")
    LiftRules.resourceNames = "i18n/got" :: LiftRules.resourceNames
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    LiftRules.setSiteMapFunc(sitemap)
    addRewritesToLiftRules

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Dispatches
    LiftRules.dispatch.append {
      case Req("image" :: secure :: name :: Nil, fileType, _) =>
        () => ImageAction.serveImage(secure, name + "." + fileType)
    }
  }

  def sitemap() = SiteMap(
    Menu("home", S ? "home") / "index",
    Menu("news", S ? "news") / "news",
    Menu("pictures", S ? "pictures") / "pictures",
    //Menu("movies", S ? "movies") / "movies", //TODO Movie/Videosektion erstellen
    Menu("about us", S ? "about.us") / "aboutus",
    Menu("contact", S ? "contact") / "contact",
    //Menu("events", S ? "events") / "events", //TODO Eventkalender erstellen
    Menu("links", S ? "links") / "links",
    //Menu("archive", S ? "archive") / "archive", //TODO Archiv für alte News erstellen (schon abgedeckt durch News sektion?)
    Menu("impressum", S ? "impressum") / "impressum"
    >> Hidden,
    Menu("admin.login", S ? "admin.login") / "admin" / "login" >> Hidden,
    Menu("admin", S ? "admin") / "admin" / "index"
    >> If(User.isAdmin_?, "You have no Permission to see this page")
    submenus (Menu("admin.news.list", S ? "admin.news.list") / "admin" / "news" / "list"
      >> If(User.isAdmin_?, "You have no Permission to see this page"),
      Menu("admin.news.new", S ? "admin.news.new") / "admin" / "news" / "new"
      >> If(User.isAdmin_?, "You have no Permission to see this page"),
      Menu("admin.news.edit", S ? "admin.news.edit") / "admin" / "news" / "edit"
      >> If(User.isAdmin_?, "You have no Permission to see this page")
      >> Hidden,
      Menu("admin.news.delete", S ? "admin.news.delete") / "admin" / "news" / "delete"
      >> If(User.isAdmin_?, "You have no Permission to see this page")
      >> Hidden,
      Menu("admin.picture.add", S ? "admin.picture.add") / "admin" / "picture" / "add"
      >> If(User.isAdmin_?, "You have no Permission to see this page"),
      Menu("admin.picture.list", S ? "admin.picture.list") / "admin" / "picture" / "list"
      >> If(User.isAdmin_?, "You have no Permission to see this page"),
      Menu("admin.picture.delete", S ? "admin.picture.delete") / "admin" / "picture" / "delete"
      >> If(User.isAdmin_?, "You have no Permission to see this page")
      >> Hidden,
      Menu("admin.picture.group", S ? "admin.picture.group") / "admin" / "picture" / "detail"
      >> If(User.isAdmin_?, "You have no Permission to see this page")
      >> Hidden,
      Menu("admin.staticpage.list", S ? "admin.staticpage.list") / "admin" / "staticpage" / "list"
      >> If(User.isAdmin_?, "You have no Permission to see this page"),
      Menu("admin.staticpage.edit", S ? "admin.staticpage.edit") / "admin" / "staticpage" / "edit"
      >> If(User.isAdmin_?, "You have no Permission to see this page")
      >> Hidden))

  def addRewritesToLiftRules() =
    LiftRules.rewrite.append {
      case RewriteRequest(
        ParsePath(List("news", id), _, _, _), _, _) =>
        RewriteResponse("news" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("news", "page", page), _, _, _), _, _) =>
        RewriteResponse("news" :: Nil, Map("page" -> page))
      case RewriteRequest(
        ParsePath(List("admin", "news", "edit", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "news" :: "edit" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "news", "delete", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "news" :: "delete" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "staticpage", "edit", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "staticpage" :: "edit" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "picture", "delete", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "picture" :: "delete" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("pictures", category), _, _, _), _, _) =>
        RewriteResponse("pictures" :: Nil, Map("category" -> category))
    }
}

