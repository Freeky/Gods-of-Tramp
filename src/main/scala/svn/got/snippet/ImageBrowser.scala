package svn.got.snippet
import _root_.scala.xml.{ NodeSeq, Text, NodeBuffer }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._
import svn.got.model._
import S._

class ImageBrowser extends DispatchSnippet {

  def dispatch: DispatchIt = _ match {
    case "currentCategory" => getCurrentCategoryName
    case "categories" => categories
    case "images" => images
    case "addCategory" => addCategory
  }

  def getCurrentCategoryName(in: NodeSeq): NodeSeq = {
    Text(S.param("category").openOr(""))
  }

  def categories(in: NodeSeq): NodeSeq = {
    val categoryName = S.param("category").openOr("")

    val currentCategory = ImageCategory.find(By(ImageCategory.name, categoryName))
    val subCategories = ImageCategory.findAll(By(ImageCategory.parent, currentCategory))

    subCategories.flatMap(cat => <a href={ "/pictures/" + cat.name.is }>{
      bind("category", in,
        "name" -> Text(cat.name.is))
    }</a>)
  }

  def images(in: NodeSeq): NodeSeq = {

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

    val outputNodes: NodeBuffer = new NodeBuffer

    def bindImage(in: NodeSeq, img: Image): NodeSeq = bind("entry", in,
      "content" -> <a href={ "/image/" + img.secure.is.toLowerCase + "/" + img.name.is }>
                     <img style="vertical-align: middle;" src={
                       "/image/" + img.secure.is.toLowerCase + "/" + img.name.is +
                         "?width=" + imageWidth + "&height=" + imageHeight
                     }/>
                   </a>)

    for (imageListId <- 0 to (images.size - 1)) {

      outputNodes.appendAll(bind("image", in,
        "entry" -> (xml => bindImage(xml, images(imageListId)))))

    }

    outputNodes.toSeq
  }

  def addCategory(in: NodeSeq): NodeSeq = {
    if (!User.isAdmin_?) return Text("")

    var categoryName = ""

    def addCategoryToDatabase(): Unit = {
      if (categoryName.isEmpty) {
        S.error("You must enter a Name!")
        return
      }

      val currentCategoryName = S.param("category").openOr("")

      val currentCategory = ImageCategory.find(By(ImageCategory.name, currentCategoryName))

      S.error(currentCategory.open_!.name)
      ImageCategory.create
        .name(categoryName)
        .parent(currentCategory.open_!)
        .save
    }

    bind("form", in,
      "name" -> SHtml.text(categoryName, categoryName = _),
      "submit" -> SHtml.submit(S ? "add", addCategoryToDatabase))
  }
}