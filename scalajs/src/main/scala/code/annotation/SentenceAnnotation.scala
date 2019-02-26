package code.annotation

import code.analysis.{PartSeqBuilder, PartialAnalysis, SentenceWithFocus}
import code.transport.lattice.{CandidateFocus, EditableSentence, EditableSentencePart, NodeTag}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SentenceApi(api: ApiService, uid: ObjId) {

  def fetchNextSentences(
      baseQuery: GetSentences,
      ignore: Seq[String],
      limit: Int = 15): Future[Seq[Sentence]] = {
    val msg = baseQuery.copy(
      exceptIds = ignore.distinct,
      limit = limit
    )

    val wrapper = SentenceRequest(
      SentenceRequest.Request.Sentences(msg)
    )

    api.sentenceCall[Sentences](wrapper).map(_.sentences)
  }

  def fetchHistory(): Future[Seq[Sentence]] = {
    val call = SentenceRequest(
      SentenceRequest.Request.History(
        ReviewHistory(15)
      )
    )
    api.sentenceCall[Sentences](call).map(_.sentences)
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

  def publishBadComment(
      data: Sentence,
      block: SentenceBlock,
      comment: String,
      focus: CandidateFocus): Unit = {
    val msg = ReportAllBad(
      sentenceId = data.id,
      focusStart = focus.start,
      focusEnd = if (focus.end <= 0) focus.start + 1 else focus.end,
      comment = comment,
      annotatorId = uid
    )

    val call = SentenceRequest(
      SentenceRequest.Request.BothBad(msg)
    )

    api.sentenceCall[Annotation](call)
  }
}

case class RecentSentenceSpan(
    content: String,
    target: Boolean
)

case class RecentSentence(id: String, spans: Seq[RecentSentenceSpan]) {
  def render() = {
    val link = AnnotationTool.routectCtl.pathFor(ViewSentence(id))
    <.li(
      ^.key := id,
      <.a.toNewWindow(link.abs(AnnotationTool.routectCtl.baseUrl).value)(
        spans.map { s =>
          <.span(
            ^.classSet("target-span" -> s.target),
            s.content
          )
        }.toTagMod
      )
    )
  }
}

object RecentSentence {
  def apply(s: Sentence): RecentSentence = {
    val spanBuffer = new ArrayBuffer[RecentSentenceSpan]()
    val bldr = new StringBuilder

    // step1: convert sentence spans to displayable objects
    var length = 0
    s.blocks.foreach { b =>
      if (b.spans.size > 1) {
        bldr.clear()
        b.spans.head.tokens.foreach(t => bldr.append(t.surface))
        val str = bldr.result()
        spanBuffer += RecentSentenceSpan(str, true)
        length += str.length
      } else {
        val str = b.spans.head.tokens.head.surface
        spanBuffer += RecentSentenceSpan(str, false)
        length += str.length
      }
    }

    // if sentence is short, we are done
    if (length < 20) {
      return RecentSentence(s.id, spanBuffer)
    }

    var toCut = length - 20

    // compression step one: discard parts of non-target tokens
    val cutCandidates = new mutable.PriorityQueue[(Int, RecentSentenceSpan)]()(Ordering.by {
      case (a, b) => -b.content.length
    })
    spanBuffer.zipWithIndex.foreach {
      case (x, i) => if (!x.target && x.content.length > 6) cutCandidates.enqueue(i -> x)
    }

    while (cutCandidates.nonEmpty && toCut > 0) {
      val (idx, el) = cutCandidates.dequeue()
      val content = el.content
      val clen = content.length
      val newLen = (clen - toCut) max 6
      val newStr = if (idx == 0) {
        "…" + content.substring(clen - newLen, clen)
      } else if (idx == spanBuffer.length - 1) {
        content.substring(0, newLen) + "…"
      } else {
        val half = newLen / 2
        content.substring(0, half) + "…" + content.substring(clen - half, clen)
      }
      spanBuffer(idx) = RecentSentenceSpan(newStr, false)
      toCut -= (clen - newLen)
    }

    if (toCut <= 0) {
      return RecentSentence(s.id, spanBuffer)
    }

    // compression step two: discard sentence from the end

    var idx = spanBuffer.length - 1
    while (idx > 0 && toCut > 0) {
      val item = spanBuffer.remove(idx)
      toCut -= item.content.length
      idx -= 1
    }

    spanBuffer.append(RecentSentenceSpan("…", false))

    RecentSentence(s.id, spanBuffer)
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
      curBlock: Option[SentenceBlock],
      showedTimestamp: Long
  ) {
    def id = data.id
  }

  private def makeCurrent(s: Sentence) = {
    CurrentSentence(
      s,
      None,
      None,
      System.currentTimeMillis()
    )
  }

  case class PageState(
      next: List[Sentence],
      current: Option[CurrentSentence],
      history: Vector[RecentSentence]
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
            case x :: xs => copy(next = xs, current = Some(makeCurrent(x)))
            case _       => copy(next = Nil, current = None)
          }
        case s @ Some(_) => copy(next = nextSentences, current = s)
      }
    }

    def addHistory(s: Sentence): PageState = {
      val newEntry = RecentSentence(s)
      val oldEntries = history.filter(_.id != s.id)
      val newHistory = newEntry +: oldEntries.slice(0, 29)
      copy(history = newHistory)
    }

    def withEditable(blk: SentenceBlock): PageState = {
      val curMod = current.map(
        x =>
          x.copy(
            editable = Some(makeEditable(x.data)),
            curBlock = Some(blk)
        ))
      copy(current = curMod)
    }

    def clearEditable(): PageState = {
      val curMod = current.map(_.copy(editable = None, curBlock = None))
      copy(current = curMod)
    }

    def finishEditingWith(edited: Sentence): PageState = {
      val curMod = current.map { c =>
        c.copy(
          data = edited,
          editable = None,
          curBlock = None
        )
      }
      copy(current = curMod)
    }

    def moveToNext(): PageState = {
      val st = next match {
        case x :: xs =>
          api.publishReview(x.id, uid)
          copy(next = xs, current = Some(makeCurrent(x)))
        case Nil => copy(next = Nil, current = None)
      }
      current match {
        case Some(s) => st.addHistory(s.data)
        case _       => st
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
                case Some(s3) if s3.data.id == s2.id =>
                  Some(s3.copy(data = s2, editable = None, curBlock = None))
                case y => y
              }
              p.copy(current = newCur)
            }
            .runNow()
        }

        s.copy(data = merged, editable = None, curBlock = None)
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
        case CommentEdit(_, comment, span) => // scope.modState(_.markAllBad(cmt, ctx, span))
          val curState = state.value.current.get //can't be None here
          val block = curState.curBlock.get

          val annMsg = Annotate(
            sentenceId = curState.data.id,
            offset = block.offset,
            annotation = "両方間違い",
            comment = comment
          )

          if (span.isDefined && comment.nonEmpty) {
            api.publishBadComment(
              curState.data,
              block,
              comment,
              span.get
            )
          }

          val edited = annotateImpl(annMsg, curState.data, curState.showedTimestamp)
          modState(_.finishEditingWith(edited))
      }
    }

    def renderHistory(data: Vector[RecentSentence]) = {
      <.ul(
        ^.cls := "sentence-history",
        data.map { rs =>
          rs.render()
        }.toVdomArray
      )
    }

    def renderSentence(state: PageImplProps, theState: PageState, s: CurrentSentence) = {
      <.div(
        ^.cls := "sentence-container",
        <.div(
          ^.cls := "sentence-info",
          AnnotationTool.routectCtl.link(ViewSentence(s.id))(
            "S-ID: ",
            s.id
          ),
          <.div(
            <.h5("History"),
            renderHistory(theState.history)
          )
        ),
        SentenceView(
          SentenceProps(
            s.data,
            state.nextAction,
            span => modState(_.withEditable(span)),
            annCmd => modState(_.applyAnnotate(annCmd)),
            state.showCounts
          ))
      )

    }

    def render(state: PageImplProps): VdomElement = {
      val theState = state.state.value
      theState.currentSentence match {
        case None => <.div("No sentences")
        case Some(s) =>
          s.editable match {
            case None =>
              renderSentence(state, theState, s)
            case Some(es) =>
              EditorFrame(ExampleEditorProps(es, handleEditEvent))
          }
      }
    }
  }

  case class PageImplProps(
      nextAction: Callback,
      state: StateSnapshot[PageState],
      showCounts: Boolean)

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
        api.fetchHistory().map { sents =>
          scope
            .modState { st =>
              val hsents = sents.map(s => RecentSentence(s))
              val both = st.history ++ hsents
              st.copy(history = both)
            }
            .runNow()
        }
      }.void

    def maybeGetNewSentences() = scope.state.flatMap { s =>
      if (s.shouldGetNew) {
        val props = scope.props.runNow()
        Callback.future(
          api
            .fetchNextSentences(props.search, s.ids)
            .map(sents => scope.modState(_.addSentences(sents))))
      } else {
        Callback.empty
      }
    }

    def render(rpp: ReviewPageProps, st: PageState): VdomElement = {
      PageImpl(
        PageImplProps(
          handler(),
          MyStateSnapshot(st)(y => scope.setState(y)),
          rpp.showCounts && isAdmin
        ))
    }
  }

  val Page = ScalaComponent
    .builder[ReviewPageProps]("AnnotationPage")
    .initialState(PageState(Nil, None, Vector.empty))
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

    private def renderEditBtn(annotation: Annotation) = {
      val showCounts = scope.props.runNow().showCounts
      val count = scope.props.runNow().block.annotations.count(_.value == "両方間違い")

      if (annotation.value == "両方間違い") {
        <.div(
          ^.cls := "other-suboption ann-selection opt-selected",
          ^.key := "opt-mis",
          ^.onClick --> annotateAs(annotation, "両方間違い"), //will clear annotation
          "両方間違い",
          <.span(
            ^.cls := "token-ann-cnt",
            count
          ).when(showCounts && count != 0)
        )
      } else {
        <.div(
          ^.cls := "other-suboption other-edit",
          ^.key := "opt-mis",
          "詳細解析",
          ^.onClick --> scope.props.flatMap(p => p.activateEdit(p.block)),
          <.span(
            ^.cls := "token-ann-cnt",
            count
          ).when(showCounts && count != 0)
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

    final case class DisplayTag(
        key: String,
        value: String,
        same: Boolean
    )

    final case class DisplayToken(token: ExampleToken, otags: Map[String, String], lenEq: Boolean) {
      def surface = token.surface

      val tags = token.tags
        .map {
          case (key, value) =>
            val sameTag = otags.get(key) match {
              case Some(`value`) => lenEq
              case _             => false
            }
            DisplayTag(key, value, sameTag)
        }
        .toSeq
        .sortBy(_.key)
    }

    final case class DisplaySpan(target: TokenSpan, other: TokenSpan, curAnn: Annotation) {
      def numAnnotations(block: SentenceBlock) = {
        block.annotations.count(_.value == newAnn)
      }

      def matchingTokens: Option[Seq[DisplayToken]] = {
        if (target.tokens.length != other.tokens.length) {
          return None
        }
        var i = 0
        val maxI = target.tokens.length

        val result = new ArrayBuffer[DisplayToken](maxI)

        while (i < maxI) {
          val t1 = target.tokens(i)
          val t2 = other.tokens(i)
          if (t1.surface != t2.surface) {
            return None
          }
          result += DisplayToken(t1, t2.tags, lenEq = true)
          i += 1
        }

        Some(result)
      }

      def tokens: Seq[DisplayToken] = {
        matchingTokens.getOrElse {
          target.tokens.map { token =>
            DisplayToken(token, Map.empty, lenEq = false)
          }
        }
      }

      val newAnn = target.index.toString

      def selected = newAnn == curAnn.value
    }

    def renderSpan(span: DisplaySpan, key: String) = {
      val showCounts = scope.props.runNow().showCounts
      val count = span.numAnnotations(scope.props.runNow().block)
      <.div(
        ^.cls := s"opt-block opt-span block-$key ann-selection",
        ^.key := key,
        ^.classSet(
          "opt-selected" -> span.selected,
          "custom-tags" -> span.target.hasCustomTags
        ),
        span.tokens.map { t =>
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
            t.tags.map { tag =>
              <.span(
                ^.cls := "token-data tag",
                ^.classSet("tag-same" -> tag.same),
                <.span(
                  ^.cls := "key",
                  tag.key,
                  ":"
                ),
                <.span(
                  ^.cls := "value",
                  tag.value
                )
              )
            }.toTagMod
          )
        }.toTagMod,
        ^.onClick --> annotateAs(span.curAnn, span.newAnn)
      )
    }

    def renderCmpBody(s1: TokenSpan, s2: TokenSpan, annotation: Annotation): TagMod = {
      val s1Dom = renderSpan(DisplaySpan(s1, s2, annotation), "s1")
      val s2Dom = renderSpan(DisplaySpan(s2, s1, annotation), "s2")
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
        <.div(
          ^.cls := "parts",
          s.blocks
            .map(b =>
              BlockView(BlockProps(b, s.id, state.activateEdit, state.annotate, state.showCounts)))
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
  case class CommentEdit(status: String, comment: String, span: Option[CandidateFocus])
      extends SentenceEditEvent
  case class ExampleEditorProps(sentence: EditableSentence, callback: SentenceEditEvent => Callback)

  case class EditorFrameState(
      sentence: EditableSentence,
      comment: String,
      focus: Option[CandidateFocus]
  )

  class EditorFrameBackend(scope: BackendScope[ExampleEditorProps, EditorFrameState]) {
    def render(state: EditorFrameState, props: ExampleEditorProps) = {
      val snap = MyStateSnapshot(SentenceWithFocus(state.sentence, state.focus))(
        s =>
          scope.modState(
            _.copy(
              sentence = s.sentence,
              focus = s.focus
            )))
      val cback = props.callback

      <.div(
        ^.cls := "fullscreen-frame",
        partialEditor.AnalysisEditor(snap)(
          <.button(
            "Apply",
            ^.onClick --> cback(AcceptEdit(state.sentence))
          ),
          <.button(
            "Cancel",
            ^.onClick --> cback(CancelEdit)
          ),
          <.input.text(
            ^.value := state.comment,
            ^.placeholder := "Comment",
            ^.onChange ==> { e: ReactEventFromInput =>
              val comment = e.target.value
              scope.modState(_.copy(comment = comment))
            }
          ),
          <.button(
            "No Candidate",
            ^.onClick --> cback(CommentEdit("Reject", state.comment, state.focus))
          )
        )
      )
    }
  }

  val EditorFrame = ScalaComponent
    .builder[ExampleEditorProps]("EditorFrame")
    .initialStateFromProps(p => EditorFrameState(p.sentence, "", None))
    .backend(s => new EditorFrameBackend(s))
    .renderBackend
    .build

  def makeSentenceDetailState(id: String): SentenceDetailState = {
    val f = apiSvc.sentenceById(id).map { so =>
      so.map { s =>
        api.publishReview(s.id, uid)
        PageState(Nil, Some(makeCurrent(s)), Vector.empty)
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
            MyStateSnapshot(s) { s =>
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
