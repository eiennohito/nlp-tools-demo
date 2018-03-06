package code.annotation

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
            <.ul(
              ^.cls := "nav-bar",
              <.li(ctl.link(AnnotatePage)("Annotate")),
              <.li(ctl.link(SentenceListPage("", 0))("Sentences")),
              <.li(ctl.link(Users)("Users")),
              <.li(ctl.link(Import)("Import"))
            ),
            <.div(
              ^.cls := "user-badge",
              ctl.link(UserInfo)("Profile")
            )
          ),
          <.div(
            ^.cls := "content",
            pc
          )
        )
      }
      .build
}
