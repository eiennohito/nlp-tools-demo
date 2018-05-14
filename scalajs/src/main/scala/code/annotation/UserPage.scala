package code.annotation

import code.annotation.Edits.Field
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.StateRW
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

case class UserPage(apisvc: ApiService) {

  class UserBackend(scope: BackendScope[Unit, AnnotationUser]) {
    def load() = Callback.future {
      val user = apisvc.user()
      user.map { u =>
        scope.setState(u)
      }
    }

    def save(value: CallbackTo[AnnotationUser]) = {
      value.flatMap { u =>
        val f = apisvc.updateUser(u).map(u => scope.setState(u))
        Callback.future(f)
      }
    }

    def render() = {
      import Lenses._

      <.div(
        Field(("Username", scope.zoom(_.name))),
        Field(("Token", scope.zoom(_.token))),
        <.button(
          ^.onClick --> save(scope.state),
          ^.title := "Save"
        )
      )
    }
  }

  val UserDisplay = ScalaComponent
    .builder[Unit]("AnnotationUser")
    .initialState(AnnotationUser())
    .backend(bs => new UserBackend(bs))
    .renderBackend
    .componentDidMount(_.backend.load())
    .build
}

object Edits {

  val TextField = ScalaComponent
    .builder[StateSnapshot[String]](displayName = "TextField")
    .stateless
    .noBackend
    .render_P { props =>
      <.input.text(
        ^.value := props.value,
        ^.onChange ==> ((e: ReactEventFromInput) => props.setState(e.target.value))
      )
    }
    .build

  val Field = ScalaComponent
    .builder[(String, StateSnapshot[String])]("Field")
    .stateless
    .noBackend
    .render_P {
      case (label, state) =>
        val id = s"textfld-${label.hashCode().toHexString}"
        <.div(
          <.label(
            ^.`for` := id,
            label
          ),
          TextField(state)(
            ^.name := id
          )
        )
    }
    .build

  val Todo = ScalaComponent.static("TODO")(<.div("TODO"))

  val Landing = ScalaComponent.static("Landing") {
    <.p("Welcome to the annotation tool. Try to annotate some sentences.")
  }
}

object Futures {

  sealed trait State
  case object NotReady extends State
  case object Ready extends State
  case class Error(e: Throwable) extends State

  private def fromFuture(f: Future[_]): State = {
    f.value match {
      case None                          => NotReady
      case Some(scala.util.Failure(thr)) => Error(thr)
      case Some(_)                       => Ready
    }
  }

  def wrapped[T](comp: ScalaComponent[T, _, _, CtorType.Props]) =
    ScalaComponent
      .builder[Future[T]]("Future")
      .initialStateFromProps(fromFuture)
      .noBackend
      .render { scope =>
        scope.state match {
          case NotReady =>
            scope.props.onComplete {
              case scala.util.Success(s) => scope.setState(Ready).runNow()
              case scala.util.Failure(t) =>
                scope
                  .setState(Error(t), Callback {
                    t.printStackTrace()
                  })
                  .runNow()
            }
            <.div("Loading...")
          case Ready =>
            comp.apply(scope.props.value.get.get)
          case Error(e) =>
            <.div("Error...")
        }
      }
      .build
}

object Lenses {

  implicit class ScalapbScopeSupport[T](val s: StateRW[_, T, _]) extends AnyVal {
    def zoom[R](f: scalapb.lenses.Lens[T, T] => scalapb.lenses.Lens[T, R]): StateSnapshot[R] = {
      val l = f(scalapb.lenses.Lens.unit[T])
      val modify: (T => T) => Callback = fn => s.modState(fn)
      StateSnapshot.zoom[T, R](l.get)(l.set).apply(s.state).apply(modify)
    }
  }

  implicit class BackendScopeSupport[T](val s: BackendScope[_, T]) extends AnyVal {
    def zoom[R](f: scalapb.lenses.Lens[T, T] => scalapb.lenses.Lens[T, R]): StateSnapshot[R] = {
      val l = f(scalapb.lenses.Lens.unit[T])
      val modify: (T => T) => Callback = fn => s.modState(fn)
      StateSnapshot.zoom[T, R](l.get)(l.set).apply(s.state.runNow()).apply(modify)
    }
  }
}
