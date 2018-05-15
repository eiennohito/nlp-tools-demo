package code.annotation

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.WebSocket

object SentenceImport {

  case class Message(idx: Int, data: String)

  case class ImporterState(messages: Vector[Message], filename: String, tags: String)

  class ImportBackend(scope: BackendScope[ApiService, ImporterState]) {

    var connection: WebSocket = null

    def init() = scope.props.map { p =>
      connection = p.importWsCall { reply =>
        scope
          .modState { state =>
            val msgs = state.messages.take(499)
            val newIdx = state.messages.headOption.map(_.idx + 1).getOrElse(0)
            val newMsgs = Vector(Message(newIdx, reply)) ++ msgs
            state.copy(messages = newMsgs)
          }
          .runNow()
      }

    }

    def close() = Callback {
      if (connection != null) {
        connection.close()
      }
    }

    def startImport(filename: String, tags: String) = Callback {
      if (connection != null) {
        val msg = if (tags.isEmpty) {
          filename
        } else {
          s"$filename,$tags"
        }
        connection.send(msg)
      }
    }

    private def renderMessages(strings: Vector[Message]) = {
      <.ul(
        ^.cls := "import-log",
        strings
          .map(
            v =>
              <.li(
                ^.key := v.idx,
                v.data
            ))
          .toVdomArray
      )
    }

    def render(s: ImporterState) = {
      <.div(
        <.h1("Importer"),
        Edits.Field(
          ("Filename",
           MyStateSnapshot(s.filename)(s => scope.modState(x => x.copy(filename = s))))),
        Edits.Field(
          (
            "Tags",
            MyStateSnapshot(s.tags)(t => scope.modState(_.copy(tags = t)))
          )),
        <.button(
          ^.onClick --> startImport(s.filename, s.tags),
          "Import"
        ),
        <.h2("Messages"),
        renderMessages(s.messages)
      )
    }

  }

  val Importer = ScalaComponent
    .builder[ApiService]("SentenceImport")
    .initialState(ImporterState(Vector.empty, "", ""))
    .backend(s => new ImportBackend(s))
    .renderBackend
    .componentDidMount(_.backend.init())
    .componentWillUnmount(_.backend.close())
    .build
}
