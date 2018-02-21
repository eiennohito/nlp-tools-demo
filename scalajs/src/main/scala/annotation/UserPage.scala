package annotation

import code.annotation.AnnotationUser
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.StateRW
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object UserPage {
  val Control = ScalaComponent
    .builder[Unit]("User")
    .stateless
    .render(_ => <.div("TODO"))
    .build
}

object Edits {
  import Lenses._

  val Field = ScalaComponent
    .builder[(String, StateSnapshot[String])]("Field")
    .stateless
    .noBackend
    .render_P {
      case (label, state) =>
        val id = label.hashCode().toString
        <.div(
          <.label(
            ^.`for` := id,
            label
          ),
          <.input.text(
            ^.name := id,
            ^.value := state.value,
            ^.onChange ==> ((e: ReactEventFromInput) => state.setState(e.target.value))
          )
        )
    }
    .build

  val UserDisplay = ScalaComponent
    .builder[Unit]("AnnotationUser")
    .initialState(AnnotationUser())
    .render { scope =>
      <.div(
        Field(("Username", scope.zoom(_.name))),
        Field(("Token", scope.zoom(_.token)))
      )
    }
    .build

  val Todo = ScalaComponent.static("TODO")(<.div("TODO"))
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
}
