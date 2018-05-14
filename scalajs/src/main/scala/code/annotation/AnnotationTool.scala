package code.annotation

import java.util.Base64

import code.analysis.PartialAnalysis
import code.transport.lattice.EditableSentence
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.{
  BaseUrl,
  Path,
  Redirect,
  Router,
  RouterConfig,
  RouterConfigDsl,
  RouterCtl
}
import org.scalajs.dom.Element

import scala.concurrent.ExecutionContext
import scala.scalajs.js.URIUtils
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

sealed trait AnnotationPage
case object LandingPage extends AnnotationPage
case object UserInfo extends AnnotationPage
case object Users extends AnnotationPage
case object Import extends AnnotationPage
case class AnnotatePage(rpp: ReviewPageProps) extends AnnotationPage
case class PartialAnalysisEditor(state: EditableSentence = EditableSentence.defaultInstance)
    extends AnnotationPage
case class SentenceListPage(search: String, skip: Int) extends AnnotationPage
case class ViewSentence(id: String) extends AnnotationPage

@JSExportTopLevel("AnnotationTool")
object AnnotationTool {
  import ExecutionContext.Implicits.global

  var routectCtl: RouterCtl[AnnotationPage] = _

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
    routectCtl = ctl

    val wrapper = Wrapper.AnnotationPageWrap(isAdmin, ctl)
    wrapper(router()).renderIntoDOM(elem)
  }

  private def routerConfig(as: ApiService, isAdmin: Boolean, uid: ObjId)(
      implicit ec: ExecutionContext) = {
    RouterConfigDsl[AnnotationPage]
      .buildConfig { dsl =>
        import dsl._
        import japgolly.scalajs.react.vdom.Implicits._

        def userListRoute =
          staticRoute(Path("#users").filter(_ => isAdmin), Users) ~> render(UserList.List(as))

        val sentenceListImpl = SentenceList(as, uid, isAdmin)

        def sentenceList =
          dynamicRouteCT[SentenceListPage](
            "#sentences" ~ ("?from=" ~ int ~ "&q=" ~ remainingPathOrBlank).option.xmap({
              case Some((skip, q)) => SentenceListPage(URIUtils.decodeURIComponent(q), skip)
              case None            => SentenceListPage("", 0)
            }) { slp =>
              if (slp.search == "" && slp.skip == 0) None
              else Some((slp.skip, URIUtils.encodeURIComponent(slp.search)))
            }) ~> { obj =>
            render(sentenceListImpl.Page(obj))
          }

        val partialAnalysisImpl = PartialAnalysis(as)

        def partialAnalysis: dsl.Rule =
          dynamicRouteCT[PartialAnalysisEditor](
            "#pana" ~ ("?state=" ~ remainingPathOrBlank).option.xmap({
              case None => PartialAnalysisEditor()
              case Some(s) =>
                try {
                  val bytes = Base64.getUrlDecoder.decode(s)
                  PartialAnalysisEditor(EditableSentence.parseFrom(bytes))
                } catch {
                  case e: Exception => PartialAnalysisEditor()
                }
            }) { editor =>
              val es = editor.state
              if (es.serializedSize == 0) None
              else Some(Base64.getUrlEncoder.encodeToString(es.toByteArray))
            }) ~> { obj =>
            render(partialAnalysisImpl.PartialAnalyser(obj.state))
          }

        val annotationImpl = SentenceAnnotation(as, uid, isAdmin)

        def viewSentence = {
          dynamicRouteCT[ViewSentence](
            ("#sentence?id=" ~ remainingPathOrBlank).xmap { str =>
              ViewSentence(URIUtils.decodeURIComponent(str))
            } { vs =>
              URIUtils.encodeURIComponent(vs.id)
            }
          ) ~> { req =>
            render(annotationImpl.OneSentence(req))
          }
        }

        def annotateRoute = {
          dynamicRouteCT[AnnotatePage](
            ("#annotate" ~ ("?" ~ remainingPathOrBlank).option).xmap { opt =>
              AnnotatePage(ReviewPageProps.fromQueryString(opt))
            } { ap =>
              ReviewPageProps.formatQueryString(ap.rpp)
            }
          ) ~> { par =>
            render(annotationImpl.Page(par.rpp))
          }
        }

        (
          staticRoute(root, LandingPage) ~> render(Edits.Landing())
            | staticRoute("#userInfo", UserInfo) ~> render(UserPage(as).UserDisplay())
            | staticRoute("#import", Import) ~> render(SentenceImport.Importer(as))
            | annotateRoute
            | sentenceList
            | viewSentence
            | partialAnalysis
            | userListRoute
        ).notFound(redirectToPage(LandingPage)(Redirect.Replace))
      }
      .logToConsole
      .setPostRender(postRenderFn)
  }

  def postRenderFn(prev: Option[AnnotationPage], next: AnnotationPage): Callback = {
    (prev, next) match {
      case (Some(PartialAnalysisEditor(s1)), PartialAnalysisEditor(s2)) =>
        val s1surf = s1.parts.map(_.surface).mkString
        val s2surf = s2.parts.map(_.surface).mkString
        if (s1surf == s2surf) {
          Callback.empty
        } else {
          RouterConfig.defaultPostRenderFn(prev, next)
        }
      case _ => RouterConfig.defaultPostRenderFn(prev, next)
    }
  }
}
