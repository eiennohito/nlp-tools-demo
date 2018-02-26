package code.annotation

import japgolly.scalajs.react.extra.router.{BaseUrl, Path, Redirect, Router, RouterConfigDsl}
import org.scalajs.dom.Element

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

sealed trait AnnotationPage
case object LandingPage extends AnnotationPage
case object UserInfo extends AnnotationPage
case object Users extends AnnotationPage
case object Import extends AnnotationPage
case object AnnotatePage extends AnnotationPage

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

    //org.scalajs.dom.console.log(s"Base URL: $baseUrl")

    val apisvc = new ApiService(url, csrfToken)
    apisvc.user().onComplete {
      case scala.util.Success(u) =>
        org.scalajs.dom.console.log(u.toString)
      case scala.util.Failure(f) =>
        f.printStackTrace()
    }

    val (router, ctl) = Router.componentAndCtl(baseUrl, routerConfig(apisvc, isAdmin, ObjId(uid)))

    val wrapper = Wrapper.AnnotationPageWrap(ctl)
    wrapper(router()).renderIntoDOM(elem)
  }

  private def routerConfig(as: ApiService, isAdmin: Boolean, uid: ObjId)(
      implicit ec: ExecutionContext) = {
    RouterConfigDsl[AnnotationPage].buildConfig { dsl =>
      import dsl._
      import japgolly.scalajs.react.vdom.Implicits._

      def userListRoute =
        staticRoute(Path("#users").filter(_ => isAdmin), Users) ~> render(UserList.List(as))

      (
        staticRoute(root, LandingPage) ~> render(Edits.Todo())
          | staticRoute("#userInfo", UserInfo) ~> render(Edits.UserDisplay())
          | staticRoute("#import", Import) ~> render(SentenceImport.Importer(as))
          | staticRoute("#annotate", AnnotatePage) ~> render(SentenceAnnotation(as, uid).Page())
          | userListRoute
      ).notFound(redirectToPage(LandingPage)(Redirect.Replace))
    }.logToConsole
  }
}
