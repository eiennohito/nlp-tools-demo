import code.AnalysisResult
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
  * @author eiennohito
  * @since 2016/09/30
  */
@JSExportTopLevel("JppAdmin")
object JppAdmin {

  @JSExport
  def init(node: dom.Element, uri: String) = {
    val cmpts = new AdminComponents(uri)
    cmpts.Main().renderIntoDOM(node)
  }
}

case class AdminState()

class AdminBackend($ : BackendScope[Unit, AdminState], ac: AdminComponents) {
  def render(s: AdminState): VdomElement = {
    <.div(
      ac.SentenceList()
    )
  }
}

case class PageState(
    start: Int,
    fixed: Boolean,
    sorting: String,
    items: Seq[AnalysisResult]
)

class ListBackend($ : BackendScope[Unit, PageState], ac: AdminComponents) {
  def init(): Callback = $.state.flatMap { state =>
    Callback.future {
      ac.items(state.start, state.fixed, state.sorting).map { res =>
        $.modState(_.copy(items = res))
      }
    }
  }

  def render(s: PageState): VdomElement = {
    <.table(
      itemTable(s.items)
    )
  }

  def oneItem(i: AnalysisResult): VdomElement = {
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

  def itemTable(items: Seq[AnalysisResult]): VdomElement = {
    <.tbody(
      items.map { i =>
        oneItem(i)
      }.toTagMod
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
      )
      .`then`({ (o: String) =>
        import prickle._
        val data = Unpickle[Seq[AnalysisResult]].fromString(o)
        p.complete(data)
      }, (o: js.Any) => p.failure(new Exception(o.toString)))

    p.future
  }

  val Main = ScalaComponent
    .builder[Unit]("Admin")
    .initialState(AdminState())
    .backend(new AdminBackend(_, self))
    .renderBackend
    .build

  val SentenceList = ScalaComponent
    .builder[Unit]("SentenceList")
    .initialState(PageState(0, false, "date-", Nil))
    .backend(s => new ListBackend(s, self))
    .renderBackend
    .componentDidMount(_.backend.init())
    .build
}
