package annotation

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl

object Wrapper {

  def AnnotationPageWrap(ctl: RouterCtl[AnnotationPage]) =
    ScalaComponent
      .builder[Unit]("PageWrap")
      .stateless
      .render_C { pc =>
        <.div(
          ^.cls := "container",
          <.div(
            ^.cls := "header",
            ctl.link(UserInfo)("User"),
            ctl.link(Users)("Users")
          ),
          <.div(
            ^.cls := "content",
            pc
          )
        )
      }
      .build
}
