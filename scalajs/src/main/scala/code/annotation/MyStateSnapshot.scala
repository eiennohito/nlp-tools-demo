package code.annotation

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.StateSnapshot

object MyStateSnapshot {

  @inline
  def apply[T](x: T)(fn: T => Callback): StateSnapshot[T] = {
    StateSnapshot(x) { (obj: Option[T], cb: Callback) =>
      Callback.sequenceOption(obj.map(y => fn(y))) >> cb
    }
  }
}
