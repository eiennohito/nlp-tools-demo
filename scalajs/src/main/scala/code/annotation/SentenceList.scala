package code.annotation

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.ExecutionContext.Implicits.global

case class SentenceList(apiBackend: ApiService, uid: ObjId, admin: Boolean) {

  private val api = new SentenceApi(apiBackend, uid)

  case class CachedSentence(data: Sentence, lifetime: Int)

  case class ListState(
      search: String = "",
      currentIds: Seq[String] = Nil,
      sentences: Map[String, Sentence] = Map.empty,
      from: Int = 0,
      total: Option[Int] = None
  )

  class ListBackend(scope: BackendScope[SentenceListPage, ListState]) {

    def init() = doSearch()

    def doSearch() = scope.state.flatMap { s =>
      val future = api.findSentences(s.search, s.from, 100).map { sents =>
        scope.modState { s2 =>
          val cache = sents.sentences.map(x => x.id -> x).toMap
          val ids = cache.keys.toVector
          s2.copy(
            currentIds = ids,
            sentences = cache,
            total = Some(sents.total)
          )
        }
      }
      Callback.future(future)
    }

    def updateSearch(state: ListState, props: SentenceListPage): Callback = {
      if (props.skip != state.from || props.search == state.search) {
        val newState = state.copy(
          search = props.search,
          from = props.skip
        )
        scope.setState(newState, doSearch())
      } else Callback.empty
    }

    def doExportTo(path: String): Callback = Callback.TODO

    private def renderTableHeader() = {
      <.thead(
        <.tr(
          <.th("Id"),
          <.th("Status"),
          <.th("Sentence")
        )
      )
    }

    private def renderEmptyRow(id: String) = {
      <.tr(
        ^.cls := "sentence-row loading",
        ^.key := id,
        <.td(
          ^.cls := "sentence-id",
          id
        ),
        <.td(
          ^.cls := "sentence-status",
          "?"
        ),
        <.td(
          ^.cls := "sentence-value loading",
          "Loading..."
        )
      )
    }

    private def statusValue(sentence: Sentence): TagMod = {
      if (admin) {
        s"${sentence.status.toString()}:${sentence.numAnnotations}"
      } else {
        val annotations = sentence.blocks.flatMap(_.annotations.find(_.annotatorId == uid))
        if (annotations.isEmpty) {
          "New"
        } else {
          "Ok"
        }
      }
    }

    private def renderSentenceBody(sentence: Sentence): TagMod = {
      sentence.blocks.map { b =>
        val isCommon = b.spans.lengthCompare(1) == 0
        val ts = b.spans.head
        <.span(
          ^.cls := "sentence-block",
          ^.classSet(
            "block-common" -> isCommon,
            "block-diff" -> !isCommon
          ),
          ts.tokens.map(_.surface).mkString
        )
      }.toTagMod
    }

    private def renderSentence(sentence: Sentence) = {
      <.tr(
        ^.cls := "sentence-row loaded",
        ^.key := sentence.id,
        <.td(
          ^.cls := "sentence-id",
          sentence.id
        ),
        <.td(
          ^.cls := "sentence-status",
          statusValue(sentence)
        ),
        <.td(
          ^.cls := "sentence-value loaded",
          renderSentenceBody(sentence)
        )
      )
    }

    private def renderTableBody(s: ListState) = {
      <.tbody(
        s.currentIds.map { id =>
          s.sentences
            .get(id)
            .map { sent =>
              renderSentence(sent)
            }
            .getOrElse(renderEmptyRow(id))
        }.toVdomArray
      )
    }

    private def renderSearchForm(query: String, skip: Int) = {
      <.div(
        ^.cls := "search-form",
        <.form(
          Edits.Field(("", StateSnapshot(query)(s => scope.modState(_.copy(search = s))))),
          <.input.submit(
            AnnotationTool.routectCtl.setOnClick(SentenceListPage(query, skip)),
            ^.value := "Search"
          ),
          ^.onSubmit --> CallbackTo(false)
        ),
        Export(this).when(admin)
      )
    }

    def render(s: ListState) = {
      <.div(
        ^.cls := "list-container",
        renderSearchForm(s.search, s.from),
        <.div(
          ^.cls := "sentences",
          <.table(
            renderTableHeader(),
            renderTableBody(s)
          )
        )
      )
    }
  }

  val Export = ScalaComponent
    .builder[ListBackend]("Export")
    .initialState("")
    .noBackend
    .render { ctx =>
      <.form(
        Edits.Field(("", StateSnapshot(ctx.state)(x => ctx.setState(x)))),
        <.input.submit(
          ^.onClick --> ctx.props.doExportTo(ctx.state),
          ^.value := "Export"
        ),
        ^.onSubmit ==> { e =>
          e.preventDefault(); CallbackTo(false)
        }
      )
    }
    .build

  val Page = ScalaComponent
    .builder[SentenceListPage]("SentenceList")
    .initialStateFromProps(p => ListState(search = p.search, from = p.skip))
    .backend(b => new ListBackend(b))
    .renderBackend
    .componentDidMount(_.backend.init())
    .componentWillReceiveProps { ev =>
      ev.backend.updateSearch(ev.state, ev.nextProps)
    }
    .build

}
