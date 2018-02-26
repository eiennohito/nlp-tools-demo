package code.annotation

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.Future
import scala.util.Random

import scala.concurrent.ExecutionContext.Implicits.global

class SentenceApi(api: ApiService, uid: ObjId) {
  def fetchNextSentences(ignore: Seq[String], limit: Int = 15): Future[Seq[Sentence]] = {
    val msg = GetSentences(
      exceptIds = ignore.distinct,
      limit = limit
    )

    val wrapper = SentenceRequest(
      SentenceRequest.Request.Sentences(msg)
    )

    api.sentenceCall[Sentences](wrapper).map(_.sentences)
  }
}

case class SentenceAnnotation(apiSvc: ApiService, uid: ObjId) {

  private val api = new SentenceApi(apiSvc, uid)

  class PageState() {
    private var nextSentences = List.empty[Sentence]
    def currentSentence = nextSentences.headOption
    def addSentences(sentences: Seq[Sentence]): PageState = {
      nextSentences = nextSentences ++ sentences.filterNot(nextSentences.contains)
      this
    }

    def moveToNext() = {
      nextSentences match {
        case _ :: xs => nextSentences = xs
        case _       => //noop
      }

      this
    }

    def ids = nextSentences.map(_.id)
  }

  case class SentenceProps(sentence: Sentence, showNext: Callback)
  case class BlockProps(block: SentenceBlock, id: String)

  class PageBackend(scope: BackendScope[Unit, PageState]) {
    def init() =
      scope.state.map { s =>
        api.fetchNextSentences(s.ids).map { sents =>
          scope.modState(_.addSentences(sents)).runNow()
        }
      }.void

    def render(state: PageState): VdomElement = {
      state.currentSentence match {
        case None    => <.div("No sentences")
        case Some(s) => SentenceView(SentenceProps(s, scope.modState(_.moveToNext())))
      }
    }
  }

  val Page = ScalaComponent
    .builder[Unit]("AnotationPage")
    .initialState(new PageState)
    .backend(sc => new PageBackend(sc))
    .renderBackend
    .componentDidMount(_.backend.init())
    .build

  class BlockViewBackend(scope: BackendScope[BlockProps, Annotation]) {

    def annotateAs(str: String) = scope.props.flatMap { p =>
      scope.modState { s =>
        if (s.value == str) {
          s.withValue("")
        } else {
          s.withValue(str)
        }
      }
    }

    def render(props: BlockProps, state: Annotation): VdomElement = {
      val spans = props.block.spans

      val content = if (spans.lengthCompare(1) == 0) {
        <.div(
          ^.cls := "block-normal",
          spans.head.tokens.map(s => <.span(s.surface)).toTagMod
        )
      } else {
        <.div(
          ^.cls := "block-diff",
          cmpBody(spans, state, props.id.hashCode() ^ props.block.offset.hashCode())
        )
      }

      <.div(
        ^.cls := "sent-block",
        ^.key := props.block.offset,
        content
      )
    }

    def renderSpan(s1: TokenSpan, annotation: Annotation, key: String) = {
      <.div(
        ^.cls := "opt-block opt-span",
        ^.key := key,
        ^.classSet(
          "opt-selected" -> (s1.index.toString == annotation.value)
        ),
        s1.tokens.map { t =>
          <.div(
            ^.cls := "token",
            <.span(
              ^.cls := "surface",
              t.surface,
            ),
            t.tags.map { tag =>
              <.span(
                ^.cls := "tag",
                <.span(
                  ^.cls := "key",
                  tag._1,
                  ":"
                ),
                <.span(
                  ^.cls := "value",
                  tag._2
                )
              )
            }.toTagMod
          )
        }.toTagMod,
        ^.onClick --> annotateAs(s1.index.toString)
      )
    }

    def renderOther(annotation: Annotation) = {
      val otherOptions = Seq(
        "両方間違い",
        "辞書漏れ",
        "文法漏れ",
        "入力：誤字脱字"
      )

      <.div(
        ^.cls := "opt-block opt-other",
        ^.key := "opt-other",
        otherOptions.map { opt =>
          <.div(
            ^.cls := "other-suboption",
            ^.classSet(
              "opt-selected" -> (annotation.value == opt)
            ),
            opt,
            ^.onClick --> annotateAs(opt)
          )
        }.toTagMod
      )
    }

    def renderCmpBody(s1: TokenSpan, s2: TokenSpan, annotation: Annotation): TagMod = {
      val s1Dom = renderSpan(s1, annotation, "s1")
      val s2Dom = renderSpan(s2, annotation, "s2")
      val otherOptions = renderOther(annotation)
      <.div(
        ^.cls := "ann-options",
        VdomArray(
          s1Dom,
          otherOptions,
          s2Dom
        )
      )
    }

    def cmpBody(spans: Seq[TokenSpan], annotation: Annotation, code: Int): TagMod = {
      spans match {
        case Seq(s1, s2) =>
          if (code % 2 == 1) renderCmpBody(s1, s2, annotation)
          else renderCmpBody(s2, s1, annotation)
        case _ =>
          <.div(s"INVALID NUMBER OF SPANS: ${spans.length}")
      }
    }
  }

  val BlockView = ScalaComponent
    .builder[BlockProps]("SentenceBlock")
    .initialStateFromProps(p => annotationState(p.block))
    .backend(s => new BlockViewBackend(s))
    .renderBackend
    .build

  val SentenceView = ScalaComponent
    .builder[SentenceProps]("Sentence")
    .stateless
    .noBackend
    .render_P { state =>
      val s = state.sentence
      <.div(
        ^.cls := "sentence",
        <.p(
          ^.cls := "sent-id",
          "S-ID: ",
          s.id
        ),
        <.div(
          ^.cls := "parts",
          s.blocks.map(b => BlockView(BlockProps(b, s.id))).toTagMod
        )
      )
    }
    .build

  def annotationState(block: SentenceBlock) = {
    block.annotations.find(_.annotatorId == uid).getOrElse(Annotation())
  }
}
