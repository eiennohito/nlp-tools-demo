package code.grpc

import io.grpc.{CallOptions, Channel}
import org.eiennohito.grpc.stream.client._
import ws.kotonoha.akane.analyzers.jumanpp.grpc.{AnalysisRequest, JumanppJumandicGrpc}
import ws.kotonoha.akane.analyzers.jumanpp.wire.Lattice
import ws.kotonoha.akane.analyzers.jumanpp.wire.lattice.LatticeDump

trait JumanppClient extends AStreamClient {
  def LatticeDump: UnaryCall[AnalysisRequest, LatticeDump]
  def TopN: UnaryCall[AnalysisRequest, Lattice]
  def TopNStream: BidiStreamCall[AnalysisRequest, Lattice]
}

object JumanppClient extends AClientCompanion[JumanppClient] {
  override def name: String = JumanppJumandicGrpc.SERVICE.getName
  override def build(channel: Channel, callOptions: CallOptions): JumanppClient = {
    val bldr = ClientBuilder(channel, callOptions)

    new JumanppClient {
      override val TopN = bldr.unary(JumanppJumandicGrpc.METHOD_TOP_N)
      override val TopNStream = bldr.bidiStream(JumanppJumandicGrpc.METHOD_TOP_NSTREAM)
      override val LatticeDump = bldr.unary(JumanppJumandicGrpc.METHOD_LATTICE_DUMP)
    }
  }
}
