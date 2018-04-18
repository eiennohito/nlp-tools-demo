package code.annotation

import java.util.Base64

import code.analysis.PartialAnalysis
import code.transport.lattice.EditableSentence
import japgolly.scalajs.react.extra.router.{BaseUrl, Path, Redirect, Router, RouterConfigDsl}
import org.scalajs.dom.Element

import scala.concurrent.ExecutionContext
import scala.scalajs.js.URIUtils
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

sealed trait AnnotationPage
case object LandingPage extends AnnotationPage
case object UserInfo extends AnnotationPage
case object Users extends AnnotationPage
case object Import extends AnnotationPage
case object AnnotatePage extends AnnotationPage
case class PartialAnalysisEditor(state: EditableSentence = EditableSentence.defaultInstance) extends AnnotationPage
case class SentenceListPage(search: String, skip: Int) extends AnnotationPage

@JSExportTopLevel("AnnotationTool")
object AnnotationTool {

  import ExecutionContext.Implicits.global

  @JSExport
  def bind(url: String, elem: Element, csrfToken: String, isAdmin: Boolean, uid: String): Unit = {
    import japgolly.scalajs.react.vdom.html_<^._
    val baseUrl = BaseUrl.fromWindowUrl { s =>
      val hash = s.indexOf('#')
      if (hash > 0) {
        s.substring(0, hash)
      } else s
    }

    val apisvc = new ApiService(url, csrfToken)
    val (router, ctl) = Router.componentAndCtl(baseUrl, routerConfig(apisvc, isAdmin, ObjId(uid)))

    val wrapper = Wrapper.AnnotationPageWrap(isAdmin, ctl)
    wrapper(router()).renderIntoDOM(elem)
  }

  private def routerConfig(as: ApiService, isAdmin: Boolean, uid: ObjId)(
      implicit ec: ExecutionContext) = {
    RouterConfigDsl[AnnotationPage].buildConfig { dsl =>
      import dsl._
      import japgolly.scalajs.react.vdom.Implicits._

      def userListRoute =
        staticRoute(Path("#users").filter(_ => isAdmin), Users) ~> render(UserList.List(as))

      def sentenceList =
        dynamicRouteCT[SentenceListPage](
          "#sentences" ~ ("?from=" ~ int ~ "&q=" ~ remainingPathOrBlank).option.xmap({
            case Some((skip, q)) => SentenceListPage(URIUtils.decodeURIComponent(q), skip)
            case None            => SentenceListPage("", 0)
          }) { slp =>
            if (slp.search == "" && slp.skip == 0) None
            else Some((slp.skip, URIUtils.encodeURIComponent(slp.search)))
          }) ~> { obj =>
          renderR(ctl => SentenceList(ctl, as, uid, isAdmin).Page(obj))
        }

      def partialAnalysis: dsl.Rule =
        dynamicRouteCT[PartialAnalysisEditor](
          "#pana" ~ ("?state=" ~ remainingPathOrBlank).option.xmap({
            case None => PartialAnalysisEditor()
            case Some(s) => try {
              val bytes = Base64.getUrlDecoder.decode(s)
              PartialAnalysisEditor(EditableSentence.parseFrom(bytes))
            } catch {
              case e: Exception => PartialAnalysisEditor()
            }
          }){ editor =>
            val es = editor.state
            if (es.serializedSize == 0) None else Some(Base64.getUrlEncoder.encodeToString(es.toByteArray))}) ~> {
          obj => renderR(ctl => PartialAnalysis(as).PartialAnalyser((obj.state, ctl)))
        }

      (
        staticRoute(root, LandingPage) ~> render(Edits.Landing())
          | staticRoute("#userInfo", UserInfo) ~> render(UserPage(as).UserDisplay())
          | staticRoute("#import", Import) ~> render(SentenceImport.Importer(as))
          | staticRoute("#annotate", AnnotatePage) ~> render(SentenceAnnotation(as, uid).Page())
          | sentenceList
          | partialAnalysis
          | userListRoute
      ).notFound(redirectToPage(LandingPage)(Redirect.Replace))
    }.logToConsole
  }
}
