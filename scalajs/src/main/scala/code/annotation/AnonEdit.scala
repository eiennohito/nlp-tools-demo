package code.annotation

import java.util.Base64

import code.analysis.{PartialAnalysis, SentenceWithFocus}
import code.transport.lattice.{EditableSentence, EditableSentencePart}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html

import scala.concurrent.ExecutionContext.Implicits.global

case class AnonEditProps(state: EditableSentence)

case class AnonEdit(api: ApiService, uid: Option[String]) {
  private val pana = PartialAnalysis(api)

  class InputFormBackend(scope: BackendScope[StateSnapshot[String], String]) {

    private val inputField = Ref[html.Input]

    def render(state: String): VdomElement = <.div(
      <.div(
        <.p("Please input a sentence to analyze:"),
        <.form(
          <.input
            .text(
              ^.value := state,
              ^.onChange ==> { ev: ReactEventFromInput =>
                scope.setState(ev.target.value)
              }
            )
            .withRef(inputField),
          <.input.submit(
            ^.value := "Analyze",
            ^.onClick --> {
              scope.props.flatMap { p =>
                p.setState(state)
              }
            }
          )
        )
      )
    )

    def init() = inputField.foreach { input =>
      input.focus()
      input.select()
    }
  }

  val InputForm = ScalaComponent
    .builder[StateSnapshot[String]]("EditInputForm")
    .initialStateFromProps(x => x.value)
    .renderBackend[InputFormBackend]
    .componentDidMount(_.backend.init())
    .build

  private def makeDefaultSentence(str: String): SentenceWithFocus = {
    SentenceWithFocus(EditableSentence(
                        parts = EditableSentencePart(surface = str) :: Nil
                      ),
                      None)
  }

  private def defaultState(props: AnonEditProps) = {
    if (props.state == EditableSentence.defaultInstance) {
      Right("")
    } else {
      Left(SentenceWithFocus(props.state, None))
    }
  }

  class AnonEditBackend(scope: BackendScope[AnonEditProps, Either[SentenceWithFocus, String]]) {
    def resetEditorFor(focus: SentenceWithFocus): Callback = {
      scope.modState { _ =>
        val stringData = focus.sentence.parts.map(_.surface).mkString
        AnonEdit.setState(Some(focus.sentence))
        Right(stringData)
      }
    }

    def reportAnalysis(sent: SentenceWithFocus): Callback = {
      val reportMsg = ReportInvalidResult(Some(sent.sentence))
      val f = api.reportAnalysisResult(reportMsg).map { _ =>
        scope.modState { v =>
          v match {
            case Left(x) if x.eq(sent) => Left(x.copy(sentence = x.sentence.copy(id = "reported")))
            case y                     => y
          }
        }
      }
      Callback.future(f)
    }
  }

  val Page = ScalaComponent
    .builder[AnonEditProps]("AnonEditPage")
    .initialStateFromProps[Either[SentenceWithFocus, String]](x => defaultState(x))
    .backend(x => new AnonEditBackend(x))
    .render { s =>
      s.state match {
        case Left(xs) =>
          val stateSnapshot = MyStateSnapshot(xs) { x =>
            AnonEdit.setState(Some(x.sentence))
            s.setState(Left(x))
          }

          <.div(
            <.span(
              ^.cls := "report-help",
              "解析報告の際、間違っている,それとも気になるところを選択して下さい"
            ),
            <.br(),
            pana.AnalysisEditor(stateSnapshot)(
              <.div(
                ^.cls := "report-controls",
                <.button(
                  "リセット",
                  ^.onClick --> s.backend.resetEditorFor(xs)
                ),
                <.button(
                  "報告する",
                  ^.onClick --> s.backend.reportAnalysis(xs)
                ),
                <.span(
                  "Thank you!".when(xs.sentence.id == "reported")
                )
              )
            )
          )

        case Right(str) =>
          val stateSnapshot = MyStateSnapshot(str) { x =>
            val sent = makeDefaultSentence(x)
            AnonEdit.setState(Some(sent.sentence))
            s.setState(Left(sent))
          }
          InputForm(stateSnapshot)
      }
    }
    .build
}

object AnonEdit {
  def getState(): EditableSentence = {
    val hash = org.scalajs.dom.window.location.hash
    if (hash == null || !hash.startsWith("#state=")) {
      EditableSentence.defaultInstance
    } else {
      val bytes = Base64.getUrlDecoder.decode(hash.substring("#state=".length))
      try {
        EditableSentence.parseFrom(bytes)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          EditableSentence.defaultInstance
      }
    }
  }

  def setState(obj: Option[EditableSentence]): Unit = {
    if (obj.isEmpty || obj.get == EditableSentence.defaultInstance) {
      org.scalajs.dom.window.location.hash = ""
      return
    }

    val bytes = obj.get.toByteArray
    val base64 = Base64.getUrlEncoder.encodeToString(bytes)
    org.scalajs.dom.window.location.hash = s"#state=$base64"
  }
}
