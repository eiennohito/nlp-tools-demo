package code.annotation

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SentenceApi(api: ApiService, uid: ObjId) {
  def fetchNextSentences(ignore: Seq[String], limit: Int = 15): Future[Seq[Sentence]] = {
    val msg = GetSentences(
      exceptIds = ignore.distinct,
      limit = limit,
      newForUser = true
    )

    val wrapper = SentenceRequest(
      SentenceRequest.Request.Sentences(msg)
    )

    api.sentenceCall[Sentences](wrapper).map(_.sentences)
  }

  def annotate(ann: Annotate): Future[Annotation] = {
    val req = SentenceRequest(
      SentenceRequest.Request.Annotate(ann)
    )
    api.sentenceCall[Annotation](req)
  }

  def findSentences(query: String, from: Int, limit: Int): Future[Sentences] = {
    val msg = GetSentences(
      limit = limit,
      from = from,
      query = query
    )

    val call = SentenceRequest(
      SentenceRequest.Request.Sentences(msg)
    )

    api.sentenceCall[Sentences](call)
  }
}

case class SentenceAnnotation(apiSvc: ApiService, uid: ObjId) {

  private val api = new SentenceApi(apiSvc, uid)

  case class PageState(stored: List[Sentence] = Nil, openTimestamp: Long = System.currentTimeMillis()) {
    def currentSentence: Option[Sentence] = stored.headOption

    def addSentences(sentences: Seq[Sentence]): PageState = {
      val curIds = ids.toSet
      val nextSentences = stored ++ sentences.filterNot(s => curIds.contains(s.id))
      PageState(nextSentences, openTimestamp)
    }

    def moveToNext(): PageState = {
      val nextSentences = stored match {
        case _ :: xs => xs
        case _       => Nil
      }

      PageState(nextSentences, System.currentTimeMillis())
    }

    def ids: List[String] = stored.map(_.id)

    def shouldGetNew: Boolean = stored.lengthCompare(5) < 0
  }

  case class SentenceProps(sentence: Sentence, showNext: Callback, showTime: Long)
  case class BlockProps(block: SentenceBlock, id: String, showTime: Long)

  class PageBackend(scope: BackendScope[Unit, PageState]) {
    def init() =
      scope.state.map { s =>
        api.fetchNextSentences(s.ids).map { sents =>
          scope.modState(_.addSentences(sents)).runNow()
        }
      }.void

    def maybeGetNewSentences() = scope.state.flatMap { s =>
      if (s.shouldGetNew) {
        Callback.future(
          api.fetchNextSentences(s.ids).map(sents => scope.modState(_.addSentences(sents))))
      } else {
        Callback.empty
      }
    }

    def render(state: PageState): VdomElement = {
      state.currentSentence match {
        case None => <.div("No sentences")
        case Some(s) =>
          SentenceView(SentenceProps(s, scope.modState(_.moveToNext()) >> maybeGetNewSentences(), state.openTimestamp))
      }
    }
  }

  val Page = ScalaComponent
    .builder[Unit]("AnotationPage")
    .initialState(new PageState)
    .backend(sc => new PageBackend(sc))
    .renderBackend
    .componentDidMount(_.backend.init())
    .shouldComponentUpdate(x =>
      CallbackTo {
        (x.currentState.currentSentence, x.nextState.currentSentence) match {
          case ((Some(s1), Some(s2))) => s1.id != s2.id
          case _                      => true
        }
    })
    .build

  class BlockViewBackend(scope: BackendScope[BlockProps, Annotation]) {

    def annotateAs(str: String) = scope.props.flatMap { p =>
      scope.modState { s =>
        val annotationValue = if (s.value == str) {
          ""
        } else {
          str
        }

        val secsEplaced = (System.currentTimeMillis() - p.showTime) / 1000.0f

        val annreq = Annotate(
          sentenceId = p.id,
          offset = p.block.offset,
          annotation = annotationValue,
          annotatorId = uid,
          duration = secsEplaced
        )

        api.annotate(annreq)

        s.withValue(annotationValue)
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
        cmpBody(spans, state, props.id.hashCode() ^ props.block.offset.hashCode())
      }

      <.div(
        ^.cls := "sent-block",
        ^.key := props.block.offset,
        content
      )
    }

    def renderSpan(s1: TokenSpan, annotation: Annotation, key: String) = {
      <.div(
        ^.cls := s"opt-block opt-span block-$key ann-selection",
        ^.key := key,
        ^.classSet(
          "opt-selected" -> (s1.index.toString == annotation.value)
        ),
        s1.tokens.map { t =>
          <.div(
            ^.cls := "token",
            <.span(
              ^.cls := "token-data surface",
              t.surface
            ),
            t.tags.toSeq
              .sortBy(_._1)
              .map { tag =>
                <.span(
                  ^.cls := "token-data tag",
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
              }
              .toTagMod
          )
        }.toTagMod,
        ^.onClick --> annotateAs(s1.index.toString)
      )
    }

    def renderOther(annotation: Annotation) = {
      val otherOptions = Seq(
        "両方間違い",
        "入力：誤字脱字",
        "入力：意味不明",
        "どうでもいい",
        "わからない"
      )

      <.div(
        ^.cls := "opt-block opt-other",
        ^.key := "opt-other",
        otherOptions.map { opt =>
          <.div(
            ^.cls := "other-suboption ann-selection",
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
        ^.cls := "ann-options block-diff",
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
    .componentWillReceiveProps { ev =>
      if (ev.nextProps.id != ev.currentProps.id) {
        ev.setState(annotationState(ev.nextProps.block))
      } else {
        Callback.empty
      }
    }
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
          s.blocks.map(b => BlockView(BlockProps(b, s.id, state.showTime))).toTagMod
        ),
        <.div(
          ^.cls := "sent-block",
          <.button(
            ^.cls := "next-sentence",
            "次の文へ",
            ^.onClick --> state.showNext
          )
        )
      )
    }
    .build

  def annotationState(block: SentenceBlock) = {
    block.annotations.find(_.annotatorId == uid).getOrElse(Annotation())
  }
}
