package code.grpc

import ws.kotonoha.akane.analyzers.jumanpp.wire.lattice.LatticeNode

object LatticeUtil {
  def surface(node: LatticeNode): String = {
    node.values.head.getString
  }
}
