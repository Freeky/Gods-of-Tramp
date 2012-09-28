package de.got.snippet
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import _root_.scala.xml.{ NodeSeq, Text }
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._
import de.got.model._
import S._
import de.got.main._
import java.io.FileOutputStream
import java.io.FileInputStream
import scala.io.Source
import scala.actors.threadpool.helpers.NanoTimer
import org.joda.time.DateTime

class ImageAction extends DispatchSnippet {
  def dispatch: DispatchIt = _ match {
    case "upload" => upload
    case "uploadWithCategory" => uploadWithCategory
    case "list" => list
    case "delete" => delete
    case "detail" => detail
  }

  def saveImage(fileName: String, mime: String, data: Array[Byte]) = {
    val image = Image.create
      .name(fileName)
      .mimeType(mime)
      .uploader(curUser.map(_.id.is).openOr(0L))
    image.save

    val file = Image.file(image)
    if (!file.createNewFile()) error("file: %s could not be created".format(file.getAbsoluteFile()))
    val fos = new FileOutputStream(file)
    fos.write(data)
    fos.close()

    image
  }

  def upload = {

    var fileBox: Box[FileParamHolder] = Empty
    val currentUser = User.find(User.currentUserId)

    def processUpload() = {
      fileBox.map(_ match {
        case FileParamHolder(_, null, _, _) => S.error("No file uploaded")
        case FileParamHolder(_, mime, fileName, data) if mime.startsWith("image/") => {
          val image = saveImage(fileName, mime, data)
          S.redirectTo("/image/" + image.secure.is.toLowerCase + "/" + image.name.is)
        }
        case _ => S.error("Invalid upload")
      })

    }

    ".fileupload" #> SHtml.fileUpload(x => fileBox = Full(x)) &
      ".submit" #> SHtml.submit(S ? "upload", processUpload)
  }

  def uploadWithCategory = {

    val currentUser = User.find(User.currentUserId)

    def processUpload() = {
      S.request.map(_.uploadedFiles.map(_ match {
        case FileParamHolder(_, null, _, _) => S.error("No file uploaded")
        case FileParamHolder(name, mime, fileName, data) if mime.startsWith("image/") => {
          val image = saveImage(fileName, mime, data)

          val currentCategoryName = S.param("category").openOr("")
          val currentCategory = ImageCategory.find(By(ImageCategory.name, currentCategoryName))
          ImageToCategory.create.image(image).category(currentCategory).save
        }
        case _ => S.error("Invalid upload")
      }))

    }
    if (User.isAdmin_?)
      ".fileupload" #> SHtml.fileUpload(_ => ()) &
        ".submit" #> SHtml.submit(S ? "upload", processUpload)
    else
      "*" #> ""
  }

  def list = {
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("30").toInt
    val entries = Image.count

    var images: List[Image] =
      for {
        foundImages <- Image.findAll(
          OrderBy(Image.id, Descending),
          StartAt((page - 1) * pagesize),
          MaxRows(pagesize))
      } yield foundImages

    // make prev page link if required
    def prev =
      if (page <= 1) "*" #> ""
      else
        "* [href]" #> "/admin/picture/list/page/%d".format(page - 1) &
          "* *" #> (S ? "newer")

    // make next page link if required
    def next =
      if (maxpages(entries, pagesize) <= page) "*" #> ""
      else "* [href]" #> "/admin/picture/list/page/%d".format(page + 1) &
        "* *" #> (S ? "older")

    // processes the given news entries to real HTML-entries 
    def bindImages = images.map(i => {
      val uploader = User.find(i.uploader.is)
      ".name [href]" #> "/image/%s/%s".format(i.secure.is.toLowerCase, i.name.is) &
        ".name *" #> i.name.is &
        ".uploader" #> Text(uploader.open_!.name.is) &
        ".id" #> Text(i.id.is.toString) &
        ".detaillink [href]" #> Image.detailLink(i)
    })

    ".entry" #> bindImages &
      ".previsious" #> prev &
      ".next" #> next
  }

  /**
   * maxpages Method
   * calculates max amount of pages for given entries,
   * why there could be max $pagesize entries per page
   * @param entries
   * @param pagesize
   * @return
   */
  def maxpages(entries: Long, pagesize: Int): Long =
    if (entries % pagesize > 0)
      (entries / pagesize) + 1
    else
      entries / pagesize

  def delete = {
    val imageId = S.param("id").open_!.toInt
    val image = Image.find(imageId).open_!

    def processDelete() = {
      ImageToCategory.findAll(By(ImageToCategory.image, image)).map(_.delete_!)
      image.delete_!
      S.error("Image is deleted!")
      S.redirectTo("/admin/picture/list")
    }

    ".submit" #> SHtml.submit(S ? "delete", processDelete) &
      ".image" #> Image.toHTML(image)
  }

  def detail = {

    val imageId = S.param("id").open_!.toInt
    val image = Image.find(imageId).open_!

    val file = Image.file(image)
    if (!file.canRead())
      error("file: %s could not be read" format (file.getAbsoluteFile()))
    val imageData = ImageIO.read(file)

    var selectedCategory = ""

    def deleteCategory(category: ImageCategory) = {
      ImageToCategory.findAll(
        By(ImageToCategory.image, image),
        By(ImageToCategory.category, category))
        .map(entry => entry.delete_!)
    }

    def showCategories = {
      image.categories.map(c =>
        ".name" #> { if (c.name.equals("")) "ROOT" else c.name.toString() } &
          ".delete" #> SHtml.submit(S ? "delete", () => deleteCategory(c)))
    }

    def listCategories = {
      SHtml.select(
        ImageCategory.findAll().map(c =>
          (c.id.toString, { if (c.name.equals("")) "ROOT" else c.name.toString() })).toSeq, Empty, selectedCategory = _)
    }

    def addCategory = {
      ImageToCategory.create
        .category(ImageCategory.find(By(ImageCategory.id, selectedCategory.toInt)))
        .image(image)
        .save()
    }

    ".image" #> Image.toHTML(image, 300, 300) &
      ".id" #> image.id.is.toString &
      ".name" #> image.name.is &
      ".height" #> imageData.getHeight.toString &
      ".width" #> imageData.getWidth.toString &
      ".uploader" #> image.uploader.getName &
      ".date" #> image.uploadDate.is.toString &
      ".showcategories" #> showCategories &
      ".listcategories" #> listCategories &
      ".addcategory" #> SHtml.submit(S ? "add", addCategory _) &
      ".deletelink [href]" #> Image.deleteLink(image)
  }
}

object ImageAction extends Logger {
  def serveImage(secure: String, name: String): Box[LiftResponse] = {

    //println("%-50s %s init".format(name, new DateTime().toString()))
    for {
      image <- Image.find(By(Image.secure, secure.toUpperCase)) if image.name.is.equals(name)
    } yield {

      //println("%-50s %s process".format(name, new DateTime().toString()))
      val widthParam = S.param("width").map(_.toInt).openOr(Int.MaxValue)
      val heightParam = S.param("height").map(_.toInt).openOr(Int.MaxValue)

      val file = Image.file(image, fileExtension(widthParam, heightParam))

      val imageBA: Array[Byte] =
        if (file.exists()) {
          val source = Source.fromFile(file)(scala.io.Codec.ISO8859)
          val byteArray = source.map(_.toByte).toArray
          source.close
          byteArray
        } else {
          val originalfile = Image.file(image)
          if (!originalfile.canRead())
            error("file: %s could not be read" format (file.getAbsoluteFile()))
          val originalImage = ImageIO.read(originalfile)

          var width =
            if (widthParam.abs < originalImage.getWidth)
              widthParam.abs
            else
              originalImage.getWidth

          var height =
            if (heightParam.abs < originalImage.getHeight)
              heightParam.abs
            else
              originalImage.getHeight

          val widthRatio = originalImage.getWidth.toDouble / width
          val heightRatio = originalImage.getHeight.toDouble / height

          if (widthRatio > heightRatio) {
            height = (originalImage.getHeight / widthRatio).toInt
          } else {
            width = (originalImage.getWidth / heightRatio).toInt
          }

          val byteStream = new ByteArrayOutputStream()

          ImageIO.write(resize(originalImage, width, height),
            image.mimeType.is.substring("image/".length), byteStream)

          val ba = byteStream.toByteArray

          if (!file.createNewFile()) error("file: %s could not be created".format(file.getAbsoluteFile()))
          val fos = new FileOutputStream(file)
          fos.write(ba)
          fos.close()

          ba
        }

      //println("%-50s %s finish".format(name, new DateTime().toString()))
      
      InMemoryResponse(
        imageBA,
        ("Content-Type" -> image.mimeType.is) :: Nil,
        Nil,
        200)
    }
  }

  def fileExtension(width: Int, height: Int): String = {
    val sw = if (width == Int.MaxValue) "" else "w%d".format(width)
    val sh = if (height == Int.MaxValue) "" else "h%d".format(height)
    sw + sh
  }
  private def resize(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    val resizedImage = new BufferedImage(width, height,
      if (image.getType == 0) BufferedImage.TYPE_INT_ARGB else image.getType);
    val g = resizedImage.createGraphics();
    g.drawImage(image, 0, 0, width, height, null);
    g.dispose();
    return resizedImage;
  }
}