import code.AnalysisResult
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactDOM}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom

import scala.concurrent.{Future, Promise}

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author eiennohito
  * @since 2016/09/30
  */
@JSExport
object JppAdmin {

  @JSExport
  def init(node: dom.Node, uri: String) = {
    val cmpts = new AdminComponents(uri)
    ReactDOM.render(cmpts.Main(), node)
  }
}

case class AdminState()

class AdminBackend($: BackendScope[Unit, AdminState], ac: AdminComponents) {
  def render(s: AdminState) = {
    <.div(
      ac.SentenceList()
    )
  }
}

case class PageState (
  start: Int,
  fixed: Boolean,
  sorting: String,
  items: Seq[AnalysisResult]
)

class ListBackend($: BackendScope[Unit, PageState], ac: AdminComponents) {
  def init() = Callback.future {
    val state = $.getInitialState()

    ac.items(state.start, state.fixed, state.sorting).map { res =>
      $.modState(_.copy(items = res))
    }
  }

  def render(s: PageState) = {
    <.table(
      itemTable(s.items)
    )
  }

  def oneItem(i: AnalysisResult) = {
    <.tr(
      <.td(
        ^.cls := "date",
        i.date
      ),
      <.td(
        ^.cls := "text",
        i.text
      ),
      <.td(
        ^.cls := "version",
        i.version
      ),
      <.td(
        ^.cls := "dic",
        i.dicVersion
      )
    )
  }

  def itemTable(items: Seq[AnalysisResult]) = {
    <.tbody(
      items.map { i =>
          oneItem(i)
      }
    )
  }
}

class AdminComponents(uri: String) { self =>
  private val jq = js.Dynamic.global.jQuery

  def items(from: Int, fixed: Boolean, sorting: String): Future[Seq[AnalysisResult]] = {
    val p = Promise[Seq[AnalysisResult]]()

    jq.ajax(
      js.Dynamic.literal(
        url = uri,
        method = "GET",
        data = js.Dynamic.literal(
          from = from,
          fixed = fixed,
          sorting = sorting
        ),
        contentType = "text/plain"
      )
    ).`then` ({ (o: String) =>
      import upickle.default._
      val data = read[Seq[AnalysisResult]](o)
      p.success(data)
    }, (o: js.Any) => p.failure(new Exception(o.toString)))

    p.future
  }

  val Main = ReactComponentB[Unit]("Admin")
    .initialState(AdminState())
    .backend(new AdminBackend(_, self))
    .renderBackend
    .build

  val SentenceList = ReactComponentB[Unit]("SentenceList")
      .initialState(PageState(0, false, "date-", Nil))
      .backend(s => new ListBackend(s, self))
      .renderBackend
      .componentDidMount(_.backend.init)
      .build
}
