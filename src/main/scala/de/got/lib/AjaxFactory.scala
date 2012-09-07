package de.got.lib
import scala.xml.Elem
import net.liftweb.http.js.JsCmd
import net.liftweb.http.S
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.proto.ProtoRules
import Helpers._
import net.liftweb.http.js.JE.JsRaw

object AjaxFactory {
  def ajaxLiveText(value: String, func: String => JsCmd, attrs: (String, String)*): Elem = {
    S.fmapFunc(S.SFuncHolder(func)) { funcName =>
      (attrs.foldLeft(<input type="text" value={ value }/>)(_ % _)) %
        ("onchange" -> SHtml.makeAjaxCall(JsRaw("'" + funcName +
          "=' + encodeURIComponent(this.value)")))
    }
  }
  
  /*
   * Using onblur event because onkeyup is not fast enough in real world environment
   */
  def ajaxLiveTextarea(value: String, func: String => JsCmd, attrs: (String, String)*): Elem = {
    S.fmapFunc(S.SFuncHolder(func)) { funcName =>
      (attrs.foldLeft(<textarea type="text">{ value }</textarea>)(_ % _)) %
        ("onblur" -> SHtml.makeAjaxCall(JsRaw("'" + funcName +
          "=' + encodeURIComponent(this.value)")))
    }
  }
}