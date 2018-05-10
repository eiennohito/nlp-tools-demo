package code.annotation

import code.analysis.{PartSeqBuilder, PartialAnalysis, SentenceWithFocus}
import code.transport.lattice.{CandidateFocus, EditableSentence, EditableSentencePart, NodeTag}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
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
}

case class SentenceAnnotation(apiSvc: ApiService, uid: ObjId) {

  private val api = new SentenceApi(apiSvc, uid)

  private val partialEditor = PartialAnalysis(apiSvc)

  case class CurrentSentence(
      data: Sentence,
      editable: Option[EditableSentence],
      showedTimestamp: Long
  ) {
    def id = data.id
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

    private def makeCurrent(s: Sentence) = {
      CurrentSentence(
        s,
        None,
        System.currentTimeMillis()
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
        case x :: xs => PageState(xs, Some(makeCurrent(x)))
        case Nil     => PageState(Nil, None)
      }
    }

    def applyAnnotate(cmd: Annotate): PageState = {
      val curMod = current.map { c =>
        val actualCmd = cmd.copy(
          annotatorId = uid,
          duration = (System.currentTimeMillis() - c.showedTimestamp) / 1000.0f
        )

        api.annotate(actualCmd) //ignore result

        val newBlocks = c.data.blocks.map { b =>
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

        val newSent = c.data.copy(blocks = newBlocks)
        c.copy(data = newSent)
      }

      copy(current = curMod)
    }

    def mergeEdits(sent: EditableSentence, scope: StateAccessPure[PageState]): PageState = {
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
      annotate: Annotate => Callback)

  case class BlockProps(
      block: SentenceBlock,
      id: String,
      activateEdit: SentenceBlock => Callback,
      annotate: Annotate => Callback)

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

    private def handleEditEvent(ev: SentenceEditEvent): Callback = {
      ev match {
        case AcceptEdit(s) => scope.modState(_.mergeEdits(s, scope))
        case CancelEdit    => scope.modState(_.clearEditable())
        case CommentEdit(cmt, ctx, span) => // scope.modState(_.markAllBad(cmt, ctx, span))
          scope.modState(_.clearEditable())
      }
    }

    def render(state: PageState): VdomElement = {
      state.currentSentence match {
        case None => <.div("No sentences")
        case Some(s) =>
          s.editable match {
            case None =>
              SentenceView(
                SentenceProps(
                  s.data,
                  scope.modState(_.moveToNext()) >> maybeGetNewSentences(),
                  span => scope.modState(_.withEditable(span)),
                  annCmd => scope.modState(_.applyAnnotate(annCmd))
                ))
            case Some(es) =>
              EditorFrame(ExampleEditorProps(es, handleEditEvent))
          }
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
        ^.onClick --> annotateAs(annotation, s1.index.toString)
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
          <.div(
            ^.key := s"opt-$opt",
            ^.cls := "other-suboption ann-selection",
            ^.classSet(
              "opt-selected" -> (annotation.value == opt)
            ),
            opt,
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
        <.p(
          ^.cls := "sent-id",
          "S-ID: ",
          s.id
        ),
        <.div(
          ^.cls := "parts",
          s.blocks
            .map(b => BlockView(BlockProps(b, s.id, state.activateEdit, state.annotate)))
            .toTagMod
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
}
