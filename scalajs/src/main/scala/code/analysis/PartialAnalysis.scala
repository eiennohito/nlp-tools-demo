package code.analysis

import code.annotation.{AnnotationPage, ApiService, Edits, PartialAnalysisEditor}
import code.transport.lattice._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.router.{RouterConfig, RouterCtl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.Event
import org.scalajs.dom.raw.Selection

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class PartialAnalysis(api: ApiService) {
  def defaultEditorState() = LatticeSubset()

  val InputString = ScalaComponent.builder[StateSnapshot[String]]("InputString")
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
    }.build

  case class EditorState(focus: Option[CandidateFocus] = None, result: Option[LatticeSubset] = None)

  class EditorBackend(scope: BackendScope[StateSnapshot[EditableSentence], EditorState]) {

    val queryId = s"backend-query-${this.hashCode()}"


    private def updateForFocus(focus: CandidateFocus): Callback = scope.props.flatMap { sentence =>
      val query = PartialAnalysisQuery(
        input = Some(sentence.value),
        focus = Some(focus)
      )

      val future = api.partialQuery(query).map{ subset =>
        scope.modState(x => x.copy(
          focus = Some(focus),
          result = x.result match {
            case None => Some(subset)
            case Some(r) => Some(r.copy(focusNodes = subset.focusNodes))
          }
        ))
      }

      Callback.future(future)
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
          api.partialQuery(PartialAnalysisQuery(Some(input))).map(l => scope.modState(_.copy(result = Some(l))).runNow())
        case _ => // noop
      }
    }

    def onFinish() = Callback {
      org.scalajs.dom.document.removeEventListener("selectionchange", selectionChangeListener)
    }


    def renderQuery(sent: EditableSentence, graphemes: Seq[String], focus: Option[CandidateFocus]) = {
      var start = 0
      val myAttr = VdomAttr[Int]("data-offset")
      val withStarts = graphemes.map { s =>
        val pos = start
        start += s.length
        (pos, s)
      }

      def isFocused(startIdx: Int) = {
        focus match {
          case None => false
          case Some(f) if f.end == 0 => f.start == startIdx
          case Some(f) => startIdx >= f.start && startIdx < f.end
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
          val contents = localGraphemes.map { case (idx, s) =>
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
        <.a("Reset",
          ^.onClick --> scope.props.flatMap(_.setState(EditableSentence.defaultInstance))
        )
      )
    }

    private def renderTop1(result: Option[LatticeSubset], offsets: Set[Int]) = {
      result match {
        case None => <.span("No analysis yet! Please wait...")
        case Some(s) =>
          var offset = 0
          <.div(
            ^.cls := "top1",
            s.top.map { n =>
              val off = offset
              offset += n.surface.length
              renderNode(n).apply(
                ^.onClick --> updateForFocus(CandidateFocus(off)),
                ^.classSet("fixed" -> offsets.contains(off))
              )
            }.toVdomArray
          )
      }
    }

    private def renderNode(n: CandidateNode) = {
      <.div(
        ^.key := n.id,
        <.span(n.surface),
        n.tags.map(t => <.span(s"${t.key}:${t.value}")).toTagMod,
        "S:",
        n.score
      )
    }


    class SequenceBuilder {
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

    private def insertReqiredNode2(node: CandidateNode, offset: Int): Callback = scope.props.flatMap { saved =>
      val sent = saved.value
      val newParts = new SequenceBuilder()

      val firstPos = offset
      val lastPos = offset + node.surface.length

      var partOffset = 0
      var wasNodeAdded = false

      def addNode() = {
        if (!wasNodeAdded) {
          wasNodeAdded = true
          newParts.addPart(EditableSentencePart(
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

      saved.setState(EditableSentence(
        id = sent.id,
        parts = newParts.result()
      ))
    }

    private def renderFocus(subset: Option[LatticeSubset]) = {
      subset match {
        case None => <.span("Please select something...")
        case Some(f) if f.focusNodes.isEmpty => <.span("No analysis candidates for the selection")
        case Some(f) =>
          <.div(
            f.focusNodes.map { n =>
              renderNode(n).apply(
                ^.onClick --> insertReqiredNode2(n, n.start)
              )
            }.toVdomArray
          )
      }
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

    def render(scope: StateSnapshot[EditableSentence], state: EditorState) = {
      val input = scope.value
      val offsets = calcInputOffsets(input)
      <.div(
        ^.cls := "analysis-editor",
        renderQuery(input, state.result.map(_.graphemes).getOrElse(Nil), state.focus),
        renderTop1(state.result, offsets),
        renderFocus(state.result)
      )
    }
  }

  val AnalysisEditor = ScalaComponent.builder[StateSnapshot[EditableSentence]]("AnalysisEditor")
    .initialState(EditorState())
    .backend(s => new EditorBackend(s))
    .renderBackend
    .componentDidMount(_.backend.onStart())
    .componentWillUnmount(_.backend.onFinish())
    .build

  def makeEditableSentence(str: String) = {
    EditableSentence(
      parts = Seq(
        EditableSentencePart(
          surface = str
        )
      )
    )
  }

  val PartialAnalyser = ScalaComponent.builder[(EditableSentence, RouterCtl[AnnotationPage])]("PartialAnalyzer")
    .initialStateFromProps(x => x._1)
    .noBackend
    .render { s =>
      val state = s.state
      val rcfg = s.props._2
      <.div(
        if (state.parts.isEmpty) {
          val snap = StateSnapshot("")(v => rcfg.set(PartialAnalysisEditor(makeEditableSentence(v))))
          InputString(snap)
        } else {
          val snap = StateSnapshot(state)(v => rcfg.set(PartialAnalysisEditor(v)))
          AnalysisEditor(snap)
        }
      )
    }.componentWillReceiveProps { ev => ev.setState(ev.state) }
    .build
}
