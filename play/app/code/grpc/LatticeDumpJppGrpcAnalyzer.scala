package code.grpc

import java.io.Closeable
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.google.inject.Provides
import com.typesafe.config.Config
import io.grpc.{CallOptions, ManagedChannel, ManagedChannelBuilder}
import javax.inject.{Inject, Provider, Singleton}
import net.codingwell.scalaguice.ScalaModule
import play.api.inject.ApplicationLifecycle
import ws.kotonoha.akane.analyzers.AsyncAnalyzer
import ws.kotonoha.akane.analyzers.jumanpp.grpc.{
  AnalysisRequest,
  JumanppGrpcConfig,
  JumanppGrpcProcess
}
import ws.kotonoha.akane.analyzers.jumanpp.wire.lattice.LatticeDump

import scala.concurrent.{ExecutionContext, Future}

trait LatticeDumpJppGrpcAnalyzer extends AsyncAnalyzer[AnalysisRequest, LatticeDump]

class LatticeDumpJppModule extends ScalaModule {
  override def configure(): Unit = {
    bind[LatticeDumpJppGrpcAnalyzer].toProvider[LatticeDumpJppGrpcAnalyzerProvider]
  }

  @Provides
  def jumanppGrpcConfig(cfg: Config): JumanppGrpcConfig = {
    import ws.kotonoha.akane.config.ScalaConfig._

    val binary = cfg.strOr("akane.jumanpp.grpc.executable", "jumanpp-jumandic-grpc")
    val config = cfg.getString("akane.jumanpp.grpc.config")
    val nthreads = cfg.intOr("akane.jumanpp.grpc.threads", 1)

    JumanppGrpcConfig(
      executable = Paths.get(binary),
      config = Paths.get(config),
      numThreads = nthreads
    )
  }
}

class LatticeDumpJppGrpcAnalyzerImpl(call: Flow[AnalysisRequest, LatticeDump, _])(
    implicit amat: Materializer)
    extends LatticeDumpJppGrpcAnalyzer {
  override def analyze(input: AnalysisRequest)(
      implicit ec: ExecutionContext): Future[LatticeDump] = {
    Source.single(input).via(call).runWith(Sink.head)
  }
}

case class PackagedAnalyzerImpl(
    impl: LatticeDumpJppGrpcAnalyzerImpl,
    chan: ManagedChannel,
    process: JumanppGrpcProcess)
    extends Closeable {
  override def close(): Unit = {
    chan.shutdown()
    process.close()
  }
}

@Singleton
class LatticeDumpJppGrpcAnalyzerProvider @Inject()(
    cfg: JumanppGrpcConfig,
    lifecycle: ApplicationLifecycle)(implicit ec: ExecutionContext, mat: Materializer)
    extends Provider[LatticeDumpJppGrpcAnalyzer] {

  private val instance = new AtomicReference[PackagedAnalyzerImpl]()

  private def makeNewInstance(): PackagedAnalyzerImpl = {
    val proc = JumanppGrpcProcess.spawn(cfg)
    val bldr = ManagedChannelBuilder.forAddress("localhost", proc.port)
    val chan: ManagedChannel = bldr.directExecutor().usePlaintext(true).build()
    val client = JumanppClient.build(chan, CallOptions.DEFAULT)
    val dumpFlow = client.LatticeDump.flow.mapError {
      case e: Throwable =>
        val v = instance.getAndSet(null)
        if (v != null) {
          v.close()
        }
        e
    }
    val impl = new LatticeDumpJppGrpcAnalyzerImpl(dumpFlow)
    val obj = new PackagedAnalyzerImpl(impl, chan, proc)
    val obj2 = instance.getAndSet(obj)
    if (obj2 != null) {
      obj2.close()
    }
    obj
  }

  override def get(): LatticeDumpJppGrpcAnalyzer = {
    val obj = instance.get()
    if (obj != null) {
      obj.impl
    } else makeNewInstance().impl
  }

  lifecycle.addStopHook(() =>
    Future {
      val i = instance.getAndSet(null)
      if (i != null) {
        i.close()
      }
  })
}
