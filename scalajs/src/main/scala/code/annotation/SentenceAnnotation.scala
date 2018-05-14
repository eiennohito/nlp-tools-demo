package code.annotation

import code.analysis.{PartSeqBuilder, PartialAnalysis, SentenceWithFocus}
import code.transport.lattice.{CandidateFocus, EditableSentence, EditableSentencePart, NodeTag}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SentenceApi(api: ApiService, uid: ObjId) {
  def fetchNextSentences(baseQuery: GetSentences, ignore: Seq[String], limit: Int = 15): Future[Seq[Sentence]] = {
    val msg = baseQuery.copy(
      exceptIds = ignore.distinct,
      limit = limit
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

  def editSentence(
      sentence: Sentence,
      edits: EditableSentence,
      duration: Float): Future[Sentence] = {
    val msg = MergeEdits(
      sentenceId = sentence.id,
      edits = Some(edits),
      annotatorId = uid,
      duration = duration
    )

    val call = SentenceRequest(
      SentenceRequest.Request.Merge(msg)
    )

    api.sentenceCall[Sentence](call)
  }

  def publishReview(sid: String, uid: ObjId): Future[Review] = {
    val msg = ReviewSentence(
      sentenceId = sid,
      annotatorId = uid
    )

    val call = SentenceRequest(
      SentenceRequest.Request.Review(msg)
    )

    api.sentenceCall[Review](call)
  }
}

case class SentenceAnnotation(apiSvc: ApiService, uid: ObjId, isAdmin: Boolean) {

  private val api = new SentenceApi(apiSvc, uid)

  private val partialEditor = PartialAnalysis(apiSvc)

  def annotateImpl(cmd: Annotate, sentence: Sentence, start: Long): Sentence = {
    val actualCmd = cmd.copy(
      annotatorId = uid,
      duration = (System.currentTimeMillis() - start) / 1000.0f
    )

    api.annotate(actualCmd) //ignore result

    val newBlocks = sentence.blocks.map { b =>
      if (b.offset == cmd.offset) {
        val filtered = b.annotations.filter(_.annotatorId != uid)
        val newAnns = if (cmd.annotation.isEmpty) {
          filtered
        } else {
          filtered :+ Annotation(
            annotatorId = actualCmd.annotatorId,
            value = actualCmd.annotation,
            comment = actualCmd.comment,
            timestamp = Some(Timestamps.now),
            duration = actualCmd.duration
          )
        }

        b.copy(annotations = newAnns)
      } else b
    }

    sentence.copy(blocks = newBlocks)
  }

  case class CurrentSentence(
      data: Sentence,
      editable: Option[EditableSentence],
      showedTimestamp: Long
  ) {
    def id = data.id
  }

  private def makeCurrent(s: Sentence) = {
    CurrentSentence(
      s,
      None,
      System.currentTimeMillis()
    )
  }

  case class PageState(
      next: List[Sentence] = Nil,
      current: Option[CurrentSentence] = None
  ) {

    private def makeEditable(sentence: Sentence) = {

      val pbld = new PartSeqBuilder

      for {
        b <- sentence.blocks
      } {
        val ann = b.annotations
          .find(_.annotatorId == uid)
          .flatMap(v => YInt.unapply(v.value))

        val span = ann match {
          case Some(i) if b.spans.length > i => b.spans(i)
          case None                          => b.spans.head
        }

        for (p <- span.tokens) {
          if (p.unit) {
            val part = EditableSentencePart(
              surface = p.surface,
              node = true,
              tags = p.tags.map { case (k, v) => NodeTag(k, v) }.toVector
            )
            pbld.addPart(part)
          } else {
            pbld.addNormal(p.surface)
          }
        }
      }

      EditableSentence(
        id = sentence.id,
        parts = pbld.result()
      )
    }

    def currentSentence: Option[CurrentSentence] = current

    def addSentences(sentences: Seq[Sentence]): PageState = {
      val curIds = ids.toSet
      val nextSentences = next ++ sentences.filterNot(s => curIds.contains(s.id))

      current match {
        case None =>
          nextSentences match {
            case x :: xs => PageState(xs, Some(makeCurrent(x)))
            case _       => PageState(Nil, None)
          }
        case s @ Some(_) => PageState(nextSentences, s)
      }
    }

    def withEditable(blk: SentenceBlock): PageState = {
      val curMod = current.map(x => x.copy(editable = Some(makeEditable(x.data))))
      copy(current = curMod)
    }

    def clearEditable(): PageState = {
      val curMod = current.map(_.copy(editable = None))
      copy(current = curMod)
    }

    def moveToNext(): PageState = {
      next match {
        case x :: xs =>
          api.publishReview(x.id, uid)
          PageState(xs, Some(makeCurrent(x)))
        case Nil     => PageState(Nil, None)
      }
    }

    def applyAnnotate(cmd: Annotate): PageState = {
      val curMod = current.map { c =>
        val newSent = annotateImpl(cmd, c.data, c.showedTimestamp)
        c.copy(data = newSent)
      }

      copy(current = curMod)
    }

    def mergeEdits(sent: EditableSentence, scope: StateSnapshot[PageState]): PageState = {
      val curMod = current.map { s =>
        val eplaced = (System.currentTimeMillis() - s.showedTimestamp) / 1000.0f
        val merged = MergeSentence.merge(s.data, sent, uid, eplaced)

        api.editSentence(s.data, sent, eplaced).map { s2 =>
          scope
            .modState { p =>
              val newCur = p.current match {
                case Some(s3) if s3.data.id == s2.id => Some(s3.copy(data = s2))
                case y                               => y
              }
              p.copy(current = newCur)
            }
            .runNow()
        }

        s.copy(data = merged, editable = None)
      }

      copy(current = curMod)
    }

    def ids: List[String] = next.map(_.id) ++ current.map(_.id).toSeq

    def shouldGetNew: Boolean = next.lengthCompare(5) < 0
  }

  case class SentenceProps(
      sentence: Sentence,
      showNext: Callback,
      activateEdit: SentenceBlock => Callback,
      annotate: Annotate => Callback,
    showCounts: Boolean
    )

  case class BlockProps(
      block: SentenceBlock,
      id: String,
      activateEdit: SentenceBlock => Callback,
      annotate: Annotate => Callback,
    showCounts: Boolean
  )

  class PageImplBackend(scope2: BackendScope[PageImplProps, Unit]) {

    def modState(fn: PageState => PageState): Callback = {
      scope2.props.flatMap(p => p.state.modState(fn))
    }

    def state = scope2.props.runNow().state

    private def handleEditEvent(ev: SentenceEditEvent): Callback = {
      ev match {
        case AcceptEdit(s) => modState(_.mergeEdits(s, state))
        case CancelEdit    => modState(_.clearEditable())
        case CommentEdit(cmt, ctx, span) => // scope.modState(_.markAllBad(cmt, ctx, span))
          modState(_.clearEditable())
      }
    }

    def render(state: PageImplProps): VdomElement = {
      val theState = state.state.value
      theState.currentSentence match {
        case None => <.div("No sentences")
        case Some(s) =>
          s.editable match {
            case None =>
              SentenceView(
                SentenceProps(
                  s.data,
                  state.nextAction,
                  span => modState(_.withEditable(span)),
                  annCmd => modState(_.applyAnnotate(annCmd)),
                  state.showCounts
                ))
            case Some(es) =>
              EditorFrame(ExampleEditorProps(es, handleEditEvent))
          }
      }
    }
  }

  case class PageImplProps(nextAction: Callback, state: StateSnapshot[PageState], showCounts: Boolean)

  val PageImpl = ScalaComponent
    .builder[PageImplProps]("PageImpl")
    .stateless
    .renderBackend[PageImplBackend]
    .build

  class PageBackend(scope: BackendScope[ReviewPageProps, PageState]) {

    def handler(): Callback = scope.modState(_.moveToNext()) >> maybeGetNewSentences()

    def init() =
      scope.state.map { s =>
        val props = scope.props.runNow()
        api.fetchNextSentences(props.search, s.ids).map { sents =>
          scope.modState(_.addSentences(sents)).runNow()
        }
      }.void

    def maybeGetNewSentences() = scope.state.flatMap { s =>
      if (s.shouldGetNew) {
        val props = scope.props.runNow()
        Callback.future(
          api.fetchNextSentences(props.search, s.ids).map(sents => scope.modState(_.addSentences(sents))))
      } else {
        Callback.empty
      }
    }

    def render(rpp: ReviewPageProps, st: PageState): VdomElement = {
      PageImpl(PageImplProps(
        handler(),
        StateSnapshot(st)(y => scope.setState(y)),
        rpp.showCounts && isAdmin
      ))
    }
  }

  val Page = ScalaComponent
    .builder[ReviewPageProps]("AnnotationPage")
    .initialState(new PageState)
    .renderBackend[PageBackend]
    .componentDidMount(_.backend.init())
    .build

  class BlockViewBackend(scope: BackendScope[BlockProps, Unit]) {
    def annotateAs(current: Annotation, newValue: String): Callback = scope.props.flatMap { p =>
      val annotationValue = if (current.value == newValue) {
        ""
      } else {
        newValue
      }

      val req = Annotate(
        sentenceId = p.id,
        offset = p.block.offset,
        annotation = annotationValue
      )

      p.annotate(req)
    }

    def renderSpan(s1: TokenSpan, annotation: Annotation, key: String) = {
      val showCounts = scope.props.runNow().showCounts
      val annValue = s1.index.toString
      val count = scope.props.runNow().block.annotations.count(_.value == annValue)
      <.div(
        ^.cls := s"opt-block opt-span block-$key ann-selection",
        ^.key := key,
        ^.classSet(
          "opt-selected" -> (annValue == annotation.value)
        ),
        s1.tokens.map { t =>
          <.div(
            ^.cls := "token",
            <.span(
              ^.cls := "token-data surface",
              t.surface,
              <.span(
                ^.cls := "token-ann-cnt",
                count
              ).when(showCounts && count != 0)
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
        ^.onClick --> annotateAs(annotation, annValue)
      )
    }

    private def renderEditBtn(annotation: Annotation) = {
      if (annotation.value == "両方間違い") {
        <.div(
          ^.cls := "opt-block opt-other other-suboption ann-selection",
          ^.key := "opt-mis",
          ^.onClick --> annotateAs(annotation, "両方間違い"), //will clear annotation
          "両方間違い"
        )
      } else {
        <.div(
          ^.cls := "other-suboption other-edit",
          ^.key := "opt-mis",
          "詳細解析",
          ^.onClick --> scope.props.flatMap(p => p.activateEdit(p.block))
        )
      }
    }

    def renderOther(annotation: Annotation) = {
      val showCounts = scope.props.runNow().showCounts

      val otherOptions = Seq(
        "入力：誤字脱字",
        "入力：意味不明",
        "どうでもいい",
        "わからない"
      )

      <.div(
        ^.cls := "opt-block opt-other",
        ^.key := "opt-other",
        renderEditBtn(annotation),
        otherOptions.map { opt =>
          val count = scope.props.runNow().block.annotations.count(_.value == opt)
          <.div(
            ^.key := s"opt-$opt",
            ^.cls := "other-suboption ann-selection",
            ^.classSet(
              "opt-selected" -> (annotation.value == opt)
            ),
            opt,
            <.span(
              ^.cls := "token-ann-cnt",
              count
            ).when(showCounts && count != 0),
            ^.onClick --> annotateAs(annotation, opt)
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

    def render(props: BlockProps): VdomElement = {
      val block = props.block
      val spans = block.spans
      val state = annotationState(block)

      val content = if (spans.lengthCompare(1) == 0) {
        <.div(
          ^.cls := "block-normal",
          spans.head.tokens.map(s => <.span(s.surface)).toTagMod
        )
      } else {
        cmpBody(spans, state, props.id.hashCode() ^ block.offset.hashCode())
      }

      <.div(
        ^.cls := "sent-block",
        ^.key := block.offset,
        content
      )
    }
  }

  private def annotationState(block: SentenceBlock) = {
    block.annotations.find(_.annotatorId == uid).getOrElse(Annotation())
  }

  val BlockView = ScalaComponent
    .builder[BlockProps]("SentenceBlock")
    .stateless
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
        AnnotationTool.routectCtl.link(ViewSentence(s.id))(
          "S-ID: ",
          s.id
        ),
        <.div(
          ^.cls := "parts",
          s.blocks
            .map(b => BlockView(BlockProps(b, s.id, state.activateEdit, state.annotate, state.showCounts)))
            .toTagMod
        ),
        <.div(
          ^.cls := "sent-block",
          <.button(
            ^.cls := "next-sentence",
            "次の文へ",
            ^.onClick --> state.showNext
          )
        ).when(state.showNext != Callback.empty)
      )
    }
    .build

  sealed trait SentenceEditEvent
  case class AcceptEdit(data: EditableSentence) extends SentenceEditEvent
  case object CancelEdit extends SentenceEditEvent
  case class CommentEdit(comment: String, context: String, span: Option[CandidateFocus])
      extends SentenceEditEvent
  case class ExampleEditorProps(sentence: EditableSentence, callback: SentenceEditEvent => Callback)

  class EditorFrameBackend(scope: BackendScope[ExampleEditorProps, SentenceWithFocus]) {
    def render(sent: SentenceWithFocus, props: ExampleEditorProps) = {
      val snap = StateSnapshot(sent)(s => scope.setState(s))
      val cback = props.callback

      <.div(
        ^.cls := "fullscreen-frame",
        partialEditor.AnalysisEditor(snap)(
          <.button(
            "Apply",
            ^.onClick --> cback(AcceptEdit(sent.sentence))
          ),
          <.button(
            "Cancel",
            ^.onClick --> cback(CancelEdit)
          ),
          <.button(
            "Reject",
            ^.onClick --> cback(CommentEdit("Reject", "", sent.focus))
          )
        )
      )
    }
  }

  val EditorFrame = ScalaComponent
    .builder[ExampleEditorProps]("EditorFrame")
    .initialStateFromProps(p => SentenceWithFocus(p.sentence, None))
    .backend(s => new EditorFrameBackend(s))
    .renderBackend
    .build

  def makeSentenceDetailState(id: String): SentenceDetailState = {
    val f = apiSvc.sentenceById(id).map { so =>
      so.map { s =>
        api.publishReview(s.id, uid)
        PageState(Nil, Some(makeCurrent(s)))
      }
    }
    SentenceDetailState(id, f)
  }

  case class SentenceDetailState(
      id: String,
      data: Future[Option[PageState]]
  )

  class SentenceDetailBackend(scope: BackendScope[ViewSentence, SentenceDetailState]) {
    def render(state: SentenceDetailState): VdomElement = {
      state.data.value match {
        case None => <.div("Loading...")
        case Some(scala.util.Failure(ex)) =>
          Callback.log(ex.getMessage)
          <.div("Failure...")
        case Some(scala.util.Success(None)) => <.div("No such sentence exist")
        case Some(scala.util.Success(Some(s))) =>
          val props = PageImplProps(
            Callback.empty,
            StateSnapshot(s) { s =>
              scope.setState(
                state.copy(data = Future.successful(Some(s)))
              )
            },
            isAdmin
          )
          PageImpl(props)
      }
    }
  }

  val OneSentence = ScalaComponent
    .builder[ViewSentence]("OneSentence")
    .initialStateFromProps(p => makeSentenceDetailState(p.id))
    .renderBackend[SentenceDetailBackend]
    .componentDidMount(x => Callback.future(x.state.data.map(_ => x.forceUpdate)))
    .build
}
