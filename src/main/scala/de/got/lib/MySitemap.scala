package de.got.lib

import net.liftweb.http.rest._
import net.liftweb.http._

object MySitemap extends RestHelper {
  serve {
    case Req("sitemap" :: Nil, _, GetRequest) =>
      XmlResponse(S.render(<lift:embed what="sitemap"/>,
        S.request.get.request).head)
  }
}
