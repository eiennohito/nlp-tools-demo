package code.analysis

import code.annotation.{AnnotationTool, ApiService, Edits, PartialAnalysisEditor}
import code.transport.lattice._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle
import japgolly.scalajs.react.extra.{LogLifecycle, StateSnapshot}
import japgolly.scalajs.react.extra.router.{RouterConfig, RouterCtl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.Event
import org.scalajs.dom.raw.Selection

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PartSeqBuilder {
  private val simpleNodeData = new java.lang.StringBuilder
  private val parts = new ArrayBuffer[EditableSentencePart]()

  private def flushPart() = {
    if (simpleNodeData.length() > 0) {
      parts += EditableSentencePart(simpleNodeData.toString)
      simpleNodeData.setLength(0)
    }
  }

  def addNormal(data: String): Unit = simpleNodeData.append(data)
  def addNormal(data: String, from: Int, to: Int): Unit = {
    if (to - from > 0) {
      simpleNodeData.append(data, from, to)
    }
  }

  def addPart(part: EditableSentencePart): Unit = {
    if (part.node) {
      flushPart()
      parts += part
    } else {
      addNormal(part.surface)
    }
  }

  def result(): Seq[EditableSentencePart] = {
    flushPart()
    parts
  }
}

case class PartialAnalysis(api: ApiService) {
  def defaultEditorState() = LatticeSubset()

  val InputString = ScalaComponent
    .builder[StateSnapshot[String]]("InputString")
    .initialStateFromProps(s => s.value)
    .noBackend
    .render { s =>
      val state = s.state
      <.div(
        Edits.Field(("Input", StateSnapshot.of(s))),
        <.button(
          ^.onClick --> s.props.setState(state),
          ^.value := "Start Analysis"
        )
      )
    }
    .build

  case class EditorState(
      query: EditableSentence,
      focus: Option[CandidateFocus] = None,
      result: Option[LatticeSubset] = None)

  class EditorBackend(scope: BackendScope[StateSnapshot[SentenceWithFocus], EditorState]) {

    val queryId = s"backend-query-${this.hashCode()}"

    def updateForFocus(focus: CandidateFocus): Callback = scope.props.flatMap { pv =>
      pv.modState(_.withFocus(focus))
    }

    def updateAnalysis(data: SentenceWithFocus): Callback = {
      val query = PartialAnalysisQuery(
        input = Some(data.sentence),
        focus = data.focus
      )

      val f = api.partialQuery(query).map { subset =>
        scope.modState { s =>
          s.copy(
            query = data.sentence,
            focus = data.focus,
            result = Some(subset)
          )
        }
      }

      Callback.future(f)
    }

    private def updateForFocusNow(focus: CandidateFocus) = {
      updateForFocus(focus).runNow()
    }

    private def handleSelectionChange(sel: Selection): Unit = {
      val anchor = sel.anchorNode

      if (!sel.isCollapsed) {
        val focusNode = sel.focusNode
        val parentOffset = anchor.parentNode.attributes.getNamedItem("data-offset")
        val focusOffset = focusNode.parentNode.attributes.getNamedItem("data-offset")
        if (parentOffset != null && focusOffset != null) {
          val v1 = parentOffset.value.toInt + sel.anchorOffset
          val v2 = focusOffset.value.toInt + sel.focusOffset
          val focus = CandidateFocus(
            start = v1 min v2,
            end = v1 max v2
          )
          updateForFocusNow(focus)
        }
      }
    }

    private val selectionChangeListener: scala.scalajs.js.Function1[Event, Unit] = { e =>
      val sel = org.scalajs.dom.window.getSelection()
      org.scalajs.dom.console.info(sel)

      val anchor = sel.anchorNode
      if (anchor != null) {
        handleSelectionChange(sel)
      }
    }

    def onStart() = Callback {
      org.scalajs.dom.document.addEventListener("selectionchange", selectionChangeListener)
      val state = scope.state.runNow()

      state.result match {
        case None =>
          val input = scope.props.runNow().value
          api
            .partialQuery(PartialAnalysisQuery(Some(input.sentence)))
            .map(l => scope.modState(_.copy(result = Some(l))).runNow())
        case _ => // noop
      }
    }

    def onFinish() = Callback {
      org.scalajs.dom.document.removeEventListener("selectionchange", selectionChangeListener)
    }

    def renderQuery(
        sent: EditableSentence,
        graphemes: Seq[String],
        focus: Option[CandidateFocus],
        children: PropsChildren) = {
      var start = 0
      val myAttr = VdomAttr[Int]("data-offset")
      val withStarts = graphemes.map { s =>
        val pos = start
        start += s.length
        (pos, s)
      }

      def isFocused(startIdx: Int) = {
        focus match {
          case None                  => false
          case Some(f) if f.end == 0 => f.start == startIdx
          case Some(f)               => startIdx >= f.start && startIdx < f.end
        }
      }

      start = 0

      <.div(
        ^.cls := "query",
        ^.id := queryId,
        sent.parts.map { p =>
          val offset = start
          val finish = start + p.surface.length
          val localGraphemes = withStarts.filter { case (i, _) => i >= start && i < finish }
          start += p.surface.length
          val contents = localGraphemes.map {
            case (idx, s) =>
              <.span(
                myAttr := idx,
                ^.cls := "grapheme",
                ^.classSet("grapheme-focused" -> isFocused(idx)),
                ^.onClick --> updateForFocus(CandidateFocus(start = idx)),
                s
              )
          }.toTagMod
          <.span(
            myAttr := offset,
            contents
          )
        }.toTagMod,
        children
      )
    }

    private def clearFocus(): Callback = {
      scope.modState { s =>
        s.copy(focus = None, result = s.result.map(_.clearFocusNodes))
      } >>
        scope.props.flatMap { p =>
          p.modState(_.withFocus(None))
        }
    }

    private def removeTagOn(start: Int, end: Int): Callback = scope.props.flatMap { pv =>
      val sent = pv.value.sentence

      val sb = new PartSeqBuilder

      var offset = 0

      for (p <- sent.parts) {
        val partStart = offset
        offset += p.surface.length

        if (partStart >= start && offset <= end) {
          sb.addNormal(p.surface)
        } else {
          sb.addPart(p)
        }
      }

      pv.modState(_.withParts(sb.result()).withFocus(None))
    }

    private def renderFocus(nodes: Seq[CandidateNode], removal: Seq[NodeRemoval]) = {
      <.div(
        ^.cls := "focus-pane",
        <.div(
          ^.cls := "pane-controls",
          <.div(
            ^.cls := "close-pane-btn",
            ^.onClick --> clearFocus()
          ),
          removal.map { r =>
            <.div(
              ^.cls := "remove-entry",
              ^.onClick --> removeTagOn(r.start, r.end),
              r.surface
            ).when(r.tagged)
          }.toTagMod,
          <.div(
            ^.cls := "remove-entry",
            "All"
          ).when(removal.count(_.tagged) > 1)
        ),
        <.div(
          ^.cls := "focus-items",
          nodes.map { n =>
            renderNode(n).apply(
              ^.onClick --> insertReqiredNode(n, n.start)
            )
          }.toVdomArray
        )
      )
    }

    case class NodeRemoval(start: Int, end: Int, surface: String, tagged: Boolean)

    private def renderTop1(
        result: Option[LatticeSubset],
        offsets: Set[Int],
        focus: CandidateFocus) = {
      result match {
        case None => <.span("No analysis yet! Please wait...")
        case Some(s) =>
          var offset = 0
          var renderedFocus = false
          val focuedNodes = new ArrayBuffer[NodeRemoval]()
          <.div(
            ^.cls := "top1",
            s.top.map { n =>
              val off = offset
              offset += n.surface.length

              if (off >= focus.start && off < focus.end) {
                focuedNodes += NodeRemoval(off, offset, n.surface, offsets.contains(off))
              }

              val selfNode = renderNode(n).apply(
                ^.onClick --> updateForFocus(CandidateFocus(off)),
                ^.classSet("fixed" -> offsets.contains(off))
              )
              if (offset >= focus.end && !renderedFocus) {
                renderedFocus = true
                <.div(
                  ^.key := n.id,
                  ^.cls := "compound-node",
                  selfNode,
                  renderFocus(s.focusNodes, focuedNodes)
                )
              } else {
                selfNode
              }

            }.toVdomArray
          )
      }
    }

    private def renderTag(tag: NodeTag) = {
      <.div(
        ^.cls := "node-tag",
        <.div(
          ^.cls := "tag-key",
          tag.key
        ),
        <.div(
          ^.cls := "tag-value",
          tag.value
        )
      )
    }

    private def renderNode(n: CandidateNode) = {
      <.div(
        ^.key := n.id,
        ^.cls := "analysis-node",
        <.span(
          ^.cls := "node-surface",
          n.surface
        ),
        n.tags.filterNot(_.value == n.surface).map(t => renderTag(t)).toTagMod
      )
    }

    private def insertReqiredNode(node: CandidateNode, offset: Int): Callback =
      scope.props.flatMap { saved =>
        val sent = saved.value.sentence
        val newParts = new PartSeqBuilder()

        val firstPos = offset
        val lastPos = offset + node.surface.length

        var partOffset = 0
        var wasNodeAdded = false

        def addNode() = {
          if (!wasNodeAdded) {
            wasNodeAdded = true
            newParts.addPart(
              EditableSentencePart(
                surface = node.surface,
                node = true,
                tags = node.tags
              ))
          }
        }

        sent.parts.foreach { p =>
          val partStart = partOffset
          val partEnd = partStart + p.surface.length
          partOffset = partEnd

          // org.scalajs.dom.console.warn(s"[$partStart $partEnd] {$firstPos, $lastPos}")

          if (partEnd <= firstPos || partStart >= lastPos) {
            // this case is not interesting: part is not in the modifying group
            newParts.addPart(p)
          } else {
            newParts.addNormal(p.surface, 0, firstPos - partStart)
            addNode()
            val newStart = lastPos - partStart
            newParts.addNormal(p.surface, newStart, p.surface.length)
          }
        }

        saved.modState(_.withParts(newParts.result()).withFocus(None))
      }

    private def calcInputOffsets(sentence: EditableSentence) = {
      var idx = 0
      val result = Set.newBuilder[Int]
      for (p <- sentence.parts) {
        if (p.node) {
          result += idx
        }
        idx += p.surface.length
      }
      result.result()
    }

    def render(
        scope: StateSnapshot[SentenceWithFocus],
        state: EditorState,
        children: PropsChildren) = {
      val input = scope.value.sentence
      val offsets = calcInputOffsets(input)
      val resolvedFocus = state.focus match {
        case None                  => CandidateFocus(Int.MaxValue, Int.MaxValue)
        case Some(f) if f.end == 0 => CandidateFocus(f.start, f.start + 1)
        case Some(f)               => f
      }
      <.div(
        ^.cls := "analysis-editor",
        renderQuery(input, state.result.map(_.graphemes).getOrElse(Nil), state.focus, children),
        renderTop1(state.result, offsets, resolvedFocus)
      )
    }
  }

  val AnalysisEditor = ScalaComponent
    .builder[StateSnapshot[SentenceWithFocus]]("AnalysisEditor")
    .initialStateFromProps(p => EditorState(query = p.value.sentence, focus = p.value.focus))
    .backend(s => new EditorBackend(s))
    .renderBackendWithChildren
    .componentDidMount(_.backend.onStart())
    .componentWillReceiveProps { p =>
      val nv = p.nextProps.value
      val s = p.state
      if (nv.sentence.equals(s.query) && nv.focus.equals(s.focus))
        Callback.empty
      else p.backend.updateAnalysis(nv)
    }
    .componentWillUnmount(_.backend.onFinish())
    .build

  private def makeEditableSentence(str: String) = {
    val es = EditableSentence(
      parts = Seq(
        EditableSentencePart(
          surface = str
        )
      )
    )
    SentenceWithFocus(es, None)
  }

  val PartialAnalyser = ScalaComponent
    .builder[EditableSentence]("PartialAnalyzer")
    .initialStateFromProps(x => SentenceWithFocus(x, None))
    .noBackend
    .render { s =>
      val state = s.state
      val rcfg = AnnotationTool.routectCtl

      def updateStateAndUrl(swf: SentenceWithFocus): Callback = {
        s.setState(swf, rcfg.set(PartialAnalysisEditor(swf.sentence)))
      }

      <.div(
        if (state.sentence.parts.isEmpty) {
          val snap = StateSnapshot("")(v => updateStateAndUrl(makeEditableSentence(v)))
          InputString(snap)
        } else {
          val snap = StateSnapshot(state)(updateStateAndUrl)
          AnalysisEditor(snap)(
            <.button("Reset",
                     ^.onClick --> updateStateAndUrl(
                       SentenceWithFocus(EditableSentence.defaultInstance, None)))
          )
        }
      )
    }
    .build
}

case class SentenceWithFocus(sentence: EditableSentence, focus: Option[CandidateFocus]) {
  def withParts(parts: Seq[EditableSentencePart]): SentenceWithFocus = {
    copy(sentence = sentence.copy(parts = parts))
  }

  def withFocus(focus: CandidateFocus): SentenceWithFocus = {
    copy(focus = Some(focus))
  }

  def withFocus(focus: Option[CandidateFocus]): SentenceWithFocus = {
    copy(focus = focus)
  }
}
