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

import de.got.model._
import de.got.snippet._
import de.got.lib._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    // Database things
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(
          Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"),
          Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    //DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    Schemifier.schemify(true, Schemifier.infoF _, Event, Offer, Date, DateRegistration, News, User, Image, StaticPage, ImageCategory, ImageToCategory, AccountType)
    S.addAround(DB.buildLoanWrapper)

    // where to search snippet
    LiftRules.addToPackages("de.got")
    LiftRules.resourceNames = "i18n/got" :: LiftRules.resourceNames
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    LiftRules.setSiteMapFunc(sitemap)

    addRewritesToLiftRules

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)
    
    // Dispatches
    LiftRules.dispatch.append {
      case Req("image" :: secure :: name :: Nil, fileType, _) =>
        () => ImageAction.serveImage(secure, "%s.%s".format(name, fileType))
    }
  }

  def sitemap() = SiteMap(
    Menu("home", S ? "home") / "index" >> LocGroup("main"),
    Menu("news", S ? "news") / "news" >> LocGroup("main"),
    Menu(new OfferLoc("offers", List("offers"), S ? "offers", LocGroup("main") :: PlaceHolder :: Nil)),
    Menu(new EventLoc("events", List("events"), S ? "events", LocGroup("main") :: PlaceHolder :: Nil)),
    Menu("pictures", S ? "pictures") / "pictures",
    Menu("about us", S ? "about.us") / "aboutus" >> LocGroup("main"),
    Menu("contact", S ? "contact") / "contact" >> LocGroup("main"),
    Menu("links", S ? "links") / "links" >> LocGroup("main"),
    Menu("options", S ? "options") / "options" >> If(() => User.loggedIn_?(), S ? "no.permission") >> LocGroup("main") >> Hidden,
    Menu("impressum", S ? "impressum") / "impressum" >> Hidden,
    Menu("agb", S ? "agb") / "agb" >> Hidden,
    Menu("login", S ? "login") / "login" >> Hidden,
    Menu("register", S ? "register") / "register" >> Hidden,
    Menu("changemail", S ? "change.mail") / "options" / "changemail" >> Hidden >> If(() => User.loggedIn_?(), S ? "no.permission"),
    Menu("changepassword", S ? "change.password") / "options" / "changepassword" >> Hidden >> If(() => User.loggedIn_?(), S ? "no.permission"),
    Menu("changenewsletter", S ? "change.newsletter") / "options" / "changenewsletter" >> Hidden >> If(() => User.loggedIn_?(), S ? "no.permission"),
    Menu("deleteaccount", S ? "delete.account") / "options" / "deleteaccount" >> Hidden >> If(() => User.loggedIn_?(), S ? "no.permission"),
    Menu("admin", S ? "admin") / "admin" / "index"
      >> LocGroup("main")
      >> If(User.isAdmin_?, S ? "no.permission")
      submenus (Menu("admin.news.list", S ? "admin.news.list") / "admin" / "news" / "list"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.news.new", S ? "admin.news.new") / "admin" / "news" / "new"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.news.edit", S ? "admin.news.edit") / "admin" / "news" / "edit"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.news.delete", S ? "admin.news.delete") / "admin" / "news" / "delete"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.offers.list", S ? "admin.offers.list") / "admin" / "offers" / "list"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.offers.new", S ? "admin.offers.new") / "admin" / "offers" / "new"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.offers.edit", S ? "admin.offers.edit") / "admin" / "offers" / "edit"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.offers.delete", S ? "admin.offers.delete") / "admin" / "offers" / "delete"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.events.list", S ? "admin.events.list") / "admin" / "events" / "list"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.events.new", S ? "admin.events.new") / "admin" / "events" / "new"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.events.edit", S ? "admin.events.edit") / "admin" / "events" / "edit"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.events.delete", S ? "admin.events.delete") / "admin" / "events" / "delete"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.picture.add", S ? "admin.picture.add") / "admin" / "picture" / "add"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.picture.list", S ? "admin.picture.list") / "admin" / "picture" / "list"
        >> If(User.isAdmin_?, "no.permission"),
        Menu("admin.picture.categories", S ? "admin.picture.categories") / "admin" / "picture" / "categories"
        >> If(User.isAdmin_?, "no.permission"),
        Menu("admin.picture.detail", S ? "admin.picture.detail") / "admin" / "picture" / "detail"
        >> If(User.isAdmin_?, "no.permission")
        >> Hidden,
        Menu("admin.picture.delete", S ? "admin.picture.delete") / "admin" / "picture" / "delete"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.staticpage.list", S ? "admin.staticpage.list") / "admin" / "staticpage" / "list"
        >> If(User.isAdmin_?, S ? "no.permission"),
        Menu("admin.staticpage.edit", S ? "admin.staticpage.edit") / "admin" / "staticpage" / "edit"
        >> If(User.isAdmin_?, S ? "no.permission")
        >> Hidden,
        Menu("admin.users", S ? "admin.users") / "admin" / "users"
        >> If(User.isAdmin_?, S ? "no.permission")),
    Menu("") / "css" / ** >> Hidden,
    Menu("") / "images" / ** >> Hidden,
    Menu("") / "js" / ** >> Hidden)

  def addRewritesToLiftRules() =
    LiftRules.statelessRewrite.append {
      case RewriteRequest(
        ParsePath(List("news", id), _, _, _), _, _) =>
        RewriteResponse("news" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("events", id), _, _, _), _, _) =>
        RewriteResponse("events" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("offers", id), _, _, _), _, _) =>
        RewriteResponse("offers" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("news", "page", page), _, _, _), _, _) =>
        RewriteResponse("news" :: Nil, Map("page" -> page))
      case RewriteRequest(
        ParsePath(List("admin", "offers", "edit", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "offers" :: "edit" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "offers", "delete", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "offers" :: "delete" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "events", "edit", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "events" :: "edit" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "events", "delete", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "events" :: "delete" :: Nil, Map("id" -> id))
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
        ParsePath(List("admin", "picture", "list", "page", page), _, _, _), _, _) =>
        RewriteResponse("admin" :: "picture" :: "list" :: Nil, Map("page" -> page))
      case RewriteRequest(
        ParsePath(List("admin", "picture", "delete", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "picture" :: "delete" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "picture", "detail", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "picture" :: "detail" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("admin", "picture", "categories", id), _, _, _), _, _) =>
        RewriteResponse("admin" :: "picture" :: "categories" :: Nil, Map("id" -> id))
      case RewriteRequest(
        ParsePath(List("pictures", category), _, _, _), _, _) =>
        RewriteResponse("pictures" :: Nil, Map("category" -> category))
      case RewriteRequest(
        ParsePath(List("admin", "users", userId), _, _, _), _, _) =>
        RewriteResponse("admin" :: "users" :: Nil, Map("id" -> userId))
    }
}

