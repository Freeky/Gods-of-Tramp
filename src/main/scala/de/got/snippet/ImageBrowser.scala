package de.got.snippet
import _root_.scala.xml.{ NodeSeq, Text, NodeBuffer }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._
import de.got.model._
import S._

class ImageBrowser extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "currentCategory" => getCurrentCategoryName
    case "categories" => categories
    case "images" => images
    case "addCategory" => addCategory
    case "listcategories" => listCategories
    case "editcategory" => editCategory
  }

  def getCurrentCategoryName = {
    "*" #> S.param("category").openOr("")
  }

  def categories = {
    val categoryName = S.param("category").openOr("")

    val currentCategory = ImageCategory.find(By(ImageCategory.name, categoryName))
    val subCategories = ImageCategory.findAll(By(ImageCategory.parent, currentCategory))

    "*" #> subCategories.map(cat =>
      ".categorylink [href]" #> { "/pictures/%s" format cat.name.is } &
        ".name" #> cat.name.is)
  }

  def images = {

    val imageWidth = S.attr("width").openOr("150").toInt
    val imageHeight = S.attr("height").openOr("150").toInt
    val categoryName = S.param("category").openOr("")

    val categoryId = ImageCategory.find(By(ImageCategory.name, categoryName))
    val images: List[Image] =
      for {
        mappedImage <- ImageToCategory.findAll(
          By(ImageToCategory.category, categoryId))
        img <- mappedImage.image.foreign
      } yield img

    "*" #> images.map(img =>
      ".imagelink [href]" #> "/image/%s/%s?width=950".format(img.secure.is.toLowerCase, img.name.is) &
        ".content [src]" #> "/image/%s/%s?width=%d&height=%d".format(img.secure.is.toLowerCase, img.name.is, imageWidth, imageHeight))
  }

  def addCategory = {

    var categoryName = ""

    def addCategoryToDatabase(): Unit = {
      if (categoryName.isEmpty) {
        S.error("You must enter a Name!")
        return
      }

      val currentCategoryName = S.param("category").openOr("")

      val currentCategory = ImageCategory.find(By(ImageCategory.name, currentCategoryName))

      S.error(currentCategoryName)
      ImageCategory.create
        .name(categoryName)
        .parent(currentCategory.open_!)
        .save
    }

    if (User.isAdmin_?)
      ".name" #> SHtml.text(categoryName, categoryName = _) &
        ".submit" #> SHtml.submit(S ? "add", addCategoryToDatabase)
    else
      "*" #> ""
  }

  def listCategories = {
    ".entry" #> ImageCategory.findAll().map(c =>
      ".id" #> c.id.is &
        ".name [href]" #> "/pictures/%s".format(c.name.is) &
        ".name *" #> { if (c.name.is.equals("")) "ROOT" else c.name.is } &
        ".parent *" #> {
          c.parent.obj match {
            case Full(cat) => { if (cat.name.is.equals("")) "ROOT" else cat.name.is }
            case _ => ""
          }
        } &
        ".edit [href]" #> "/admin/picture/categories/%d".format(c.id.is))
  }

  def editCategory = {
    val categoryBox = ImageCategory.find(S.param("id").openOr("0").toLong)
    var selectedParent = "0"

    categoryBox match {
      case Full(category) => {
        def buildCategoryList(): List[(String, String)] = {
          ("0", "None") :: ImageCategory.findAll(NotBy(ImageCategory.id, category.id)).map(c => (c.id.toString, c.name.is))
        }
        def buildCategoryBox(): Box[String] = {
          category.parent.obj match {
            case Full(c) => Full(c.id.is.toString)
            case _ => Full("0")
          }
        }
        
        def saveCategory() = {
          if(!selectedParent.equals("0")) {
            category.parent(selectedParent.toLong)
          } else {
            category.parent(Empty)
          }
          category.save()
        }
        
        def deleteCategory() = {
          ImageToCategory.findAll(By(ImageToCategory.category, category)).map(i2c => i2c.delete_!)
          category.delete_!
        }

        ".name" #> SHtml.text(category.name, category.name(_)) &
          ".parent" #> SHtml.select(buildCategoryList, buildCategoryBox, selectedParent = _) &
          ".submit" #> SHtml.submit(S ? "edit", saveCategory) &
          ".delete" #> SHtml.submit(S ? "delete", deleteCategory)
      }
      case _ => "*" #> ""
    }
  }
}