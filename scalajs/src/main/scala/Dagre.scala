import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSName}

/**
  * @author eiennohito
  * @since 2016/09/28
  */

@js.native
trait GraphConfig extends js.Object {
  def rankdir: String = js.native
}

object GraphConfig {
  def apply(): GraphConfig =
    js.Dynamic
      .literal(
        rankdir = "lr"
      )
      .asInstanceOf[GraphConfig]
}

@js.native
trait NodeConfig extends js.Object {}

object NodeConfig {
  def apply(
      id: Int,
      label: js.Any = "",
      cls: String = "simple",
      tooltip: js.Any = ""
  ): NodeConfig =
    js.Dynamic
      .literal(
        id = s"node-$id",
        label = label,
        labelType = "html",
        `class` = cls,
        padding = 0,
        tooltip = tooltip
      )
      .asInstanceOf[NodeConfig]
}

@js.native
trait EdgeConfig extends js.Object {
  def label: String = js.native
}

object EdgeConfig {
  def apply(
      label: js.Any,
      cls: String = ""
  ): EdgeConfig =
    js.Dynamic
      .literal(
        label = label,
        labelType = "html",
        `class` = cls
      )
      .asInstanceOf[EdgeConfig]
}

@js.native
@JSGlobal("dagreD3.graphlib.Graph")
class DagreGraph extends js.Object {
  def setGraph(graphConfig: GraphConfig): DagreGraph = js.native

  def setNode(title: String, cfg: NodeConfig): DagreGraph = js.native

  def setEdge(begin: String, end: String, cfg: EdgeConfig): DagreGraph = js.native
}

@js.native
@JSGlobal("dagreD3")
object DagreD3 extends js.Object {
  def render(): js.Dynamic = js.native
}
