package code.annotation

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl

object Wrapper {

  def AnnotationPageWrap(isAdmin: Boolean, ctl: RouterCtl[AnnotationPage]) =
    ScalaComponent
      .builder[Unit]("PageWrap")
      .stateless
      .render_C { children =>
        <.div(
          ^.cls := "container",
          <.div(
            ^.cls := "header",
            <.ul(
              ^.cls := "nav-bar",
              <.li(ctl.link(AnnotatePage(ReviewPageProps.default))("Annotate")),
              <.li(ctl.link(SentenceListPage("", 0))("Sentences")),
              <.li(ctl.link(Users)("Users")).when(isAdmin),
              <.li(ctl.link(Import)("Import")).when(isAdmin),
              <.li(ctl.link(PartialAnalysisEditor())("AEdit"))
            ),
            <.ul(
              ^.cls := "nav-bar",
              <.li(ctl.link(UserInfo)("Profile")),
              <.li(
                <.a(
                  ^.href := "./atool/logout",
                  "Logout"
                )
              )
            )
          ),
          <.div(
            ^.cls := "content",
            children
          )
        )
      }
      .build
}
