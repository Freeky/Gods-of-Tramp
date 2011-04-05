package svn.got.snippet
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
import svn.got.model._
import S._

class ImageAction extends DispatchSnippet {
  def dispatch: DispatchIt = _ match {
    case "upload" => upload
    case "uploadWithCategory" => uploadWithCategory
    case "list" => list
    case "delete" => delete
  }

  def upload(in: NodeSeq): NodeSeq = {

    var fileBox: Box[FileParamHolder] = Empty
    val currentUser = User.find(User.currentUserId)

    def processUpload() = {
      fileBox match {
        case Full(FileParamHolder(_, null, _, _)) => S.error("No file uploaded")
        case Full(FileParamHolder(_, mime, fileName, data)) if mime.startsWith("image/") => {
          val image = Image.create
            .blob(data)
            .name(fileName)
            .mimeType(mime)
            .uploader(currentUser)
          image.save
          S.redirectTo("/image/" + image.secure.is.toLowerCase + "/" + image.name.is)
        }
        case _ => S.error("Invalid upload")
      }

    }

    bind("form", in,
      "fileupload" -> SHtml.fileUpload(x => fileBox = Full(x)),
      "submit" -> SHtml.submit(S ? "upload", processUpload))
  }

  def uploadWithCategory(in: NodeSeq): NodeSeq = {
    if (!User.isAdmin_?) return Text("")

    var fileBox: Box[FileParamHolder] = Empty
    val currentUser = User.find(User.currentUserId)

    def processUpload() = {
      fileBox match {
        case Full(FileParamHolder(_, null, _, _)) => S.error("No file uploaded")
        case Full(FileParamHolder(_, mime, fileName, data)) if mime.startsWith("image/") => {
          val image = Image.create
            .blob(data)
            .name(fileName)
            .mimeType(mime)
            .uploader(currentUser)
          image.save

          val currentCategoryName = S.param("category").openOr("")
          val currentCategory = ImageCategory.find(By(ImageCategory.name, currentCategoryName))
          ImageToCategory.create.image(image).category(currentCategory).save
        }
        case _ => S.error("Invalid upload" + fileBox.toString)
      }

    }
    bind("form", in,
      "fileupload" -> SHtml.fileUpload(x => fileBox = Full(x)),
      "submit" -> SHtml.submit(S ? "upload", processUpload))
  }
  def list(in: NodeSeq): NodeSeq = {
    val page = S.param("page").openOr("1").toInt
    val pagesize = S.param("pagesize").openOr("50").toInt
    val entries = Image.count

    var images: List[Image] =
      for {
        foundImages <- Image.findAll(
          OrderBy(Image.id, Descending),
          StartAt((page - 1) * pagesize),
          MaxRows(pagesize))
      } yield foundImages

    // make prev page link if required
    def prev(in: NodeSeq) =
      if (page <= 1) Text("")
      else <a href={ "/admin/picture/list/page/" + (page - 1) }>{ S ? "newer" }</a>

    // make next page link if required
    def next(in: NodeSeq) =
      if (maxpages(entries, pagesize) <= page) Text("")
      else <a href={ "/admin/picture/list/page/" + (page + 1) }>{ S ? "older" }</a>

    // processes the given news entries to real HTML-entries 
    def bindImages(in: NodeSeq): NodeSeq = images.flatMap(i => {
      val uploader = User.find(i.uploader.is)
      bind("entry", in,
        "name" -> Text(i.name.is),
        "uploader" -> Text(uploader.open_!.name.is),
        "id" -> Text(i.id.is.toString),
        "link" -> <a href={ "/image/" + i.secure.is.toLowerCase + "/" + i.name.is }>Link</a>,
        "deletelink" -> <a href={ "/admin/picture/delete/" + i.id.is }>Delete</a>)
    })

    bind("picture", in,
      "entries" -> bindImages _,
      "previsious" -> prev _,
      "next" -> next _)
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

  def delete(in: NodeSeq): NodeSeq = {
    val imageId = S.param("id").open_!.toInt
    val image = Image.find(imageId).open_!
    
    def processDelete() = {
    	ImageToCategory.findAll(By(ImageToCategory.image, image)).map(_.delete_!)
    	image.delete_!
    	S.redirectTo("/admin/picture/list")
    	S.error("Image is deleted!")
    }
    
    bind("delete", in,
    		"submit" -> SHtml.submit(S ? "delete", processDelete),
    		"image" -> Image.toHTML(image))
  }
}

object ImageAction extends Logger {
  def serveImage(secure: String, name: String): Box[LiftResponse] = {

    for {
      image <- Image.find(By(Image.secure, secure.toUpperCase)) if image.name.is.equals(name)
    } yield {
      val inputStream = new ByteArrayInputStream(image.blob.is)
      val originalImage = ImageIO.read(inputStream)

      val widthParam = S.param("width").openOr(Int.MaxValue.toString).toInt
      var width =
        if (widthParam.abs < originalImage.getWidth)
          widthParam.abs
        else
          originalImage.getWidth

      val heightParam = S.param("height").openOr(Int.MaxValue.toString).toInt
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

      InMemoryResponse(
        byteStream.toByteArray,
        ("Content-Type" -> image.mimeType.is) :: Nil,
        Nil,
        200)
    }
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