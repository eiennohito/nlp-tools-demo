package code

import java.time.LocalTime
import java.util.concurrent.TimeUnit

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorRefFactory, ActorSystem, PoisonPill, Props, Scheduler}
import akka.util.Timeout
import com.google.inject._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import org.apache.commons.io.IOUtils
import org.joda.time.DateTime
import play.api.Configuration
import ws.kotonoha.akane.analyzers.jumanpp.{JumanppActor, JumanppConfig}
import ws.kotonoha.akane.analyzers.jumanpp.wire.Lattice
import ws.kotonoha.akane.io.Charsets

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * @author eiennohito
  * @since 2016/09/28
  */
class JumanppModule extends Module {
  override def configure(binder: Binder): Unit = {}

  @Provides
  @Singleton
  def jsf(cfg: Configuration, asys: ActorSystem): JumanppService = {
    val fact = new JumanppServiceFactory(cfg.underlying, asys.scheduler, asys)(asys.dispatcher)
    fact
  }
}

trait JumanppService {
  def actor: ActorRef
  def version: String

  def analyze(data: String)(implicit ec: ExecutionContext): Future[Lattice] = {
    import scala.concurrent.duration._
    import akka.pattern.ask
    implicit val timeout: Timeout = 10.seconds
    val query = JumanppActor.AnalyzeRequest(Nil, data)
    actor.ask(query).map {
      case JumanppActor.AnalysisSuccess(_, res) => res
      case JumanppActor.AnalysisFailure(_, t) => throw t
    }
  }
}

class JumanppServiceImpl(val version: String, val actor: ActorRef) extends JumanppService

class JumanppServiceFactory(cfg: Config, sched: Scheduler, afact: ActorRefFactory)(implicit ec: ExecutionContext) extends JumanppService with StrictLogging {
  private val jppConfig = JumanppConfig(cfg)
  private var internal: Future[JumanppService] = makeInstance()

  import scala.concurrent.duration._

  private def getVersion(): Option[String] = {
    import scala.collection.JavaConverters._
    val jpc = JumanppConfig(cfg)
    logger.debug(s"jumnapp config: $jpc")
    val pb = new ProcessBuilder(jpc.executable, "-v")
    try {
      val proc = pb.start()
      proc.waitFor(1, TimeUnit.SECONDS)
      val out = IOUtils.readLines(proc.getInputStream, Charsets.utf8)
      out.asScala.headOption
    } catch {
      case e: Exception =>
        logger.error("could not get version of juman++", e)
        None
    }
  }

  private def makeInstance() = {
    import scala.concurrent.duration._
    val props = Props(new JumanppActor(cfg))
    val realProps = Props(new TokenBasedParent(props, 3, 9.seconds))
    getVersion() match {
      case Some(v) =>
        val aref = afact.actorOf(realProps)
        Future.successful(new JumanppServiceImpl(v, aref))
      case None =>
        Future.failed(new Exception("could not get version of juman++"))
    }
  }



  override def actor: ActorRef = Await.result(internal.map(_.actor), 10.seconds)
  override def version: String = Await.result(internal.map(_.version), 10.seconds)
  override def analyze(data: String)(implicit ec: ExecutionContext): Future[Lattice] = internal.flatMap(_.analyze(data))
}

class TokenBasedActor(childFactory: Props) extends Actor {
  private val child = context.actorOf(childFactory, "child")

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    context.parent ! TokenBasedActor.Token
  }

  private var cachedSender: ActorRef = null

  override def receive: Receive = {
    case msg =>
      val sndr = sender()
      if (sndr == child && cachedSender != null) {
        cachedSender ! msg
        cachedSender = null
        context.parent ! TokenBasedActor.Token

      } else if (cachedSender == null && sndr != child) {
        child ! msg
        cachedSender = sender()
      } else throw new Exception(s"could not have recieved $msg")
  }
}

object TokenBasedActor {
  case object Token
}

class TokenBasedParent(child: Props, number: Int, maxProcessTime: FiniteDuration) extends Actor {
  private var avalable: List[(ActorRef, DateTime)] = Nil
  private var waitingMessages: List[(Any, ActorRef, DateTime)] = Nil

  case object CleanBuffers

  override def preStart(): Unit = {
    context.system.scheduler.schedule(maxProcessTime, maxProcessTime, self, CleanBuffers)(context.dispatcher)
  }

  def trySendBuffer() = {
    val now = DateTime.now()

    @tailrec
    def rec(): Unit = {
      waitingMessages match {
        case (msg, sender, time) :: xs =>
          waitingMessages = xs
          if (time.plus(maxProcessTime.toMillis).isBefore(now)) {
            rec() //drop message
          } else {
            val (actor, _) = avalable.head
            avalable = avalable.tail
            actor.tell(msg, sender)

          }
        case Nil => //do nothing
      }
    }

    rec()
  }

  def cleanBuffers() = {
    val (good, stale) = avalable.partition { case (_, t) => DateTime.now().minusHours(1).isBefore(t)}
    avalable = good
    stale.foreach {
      case (a, _) => a ! PoisonPill
    }
  }

  override def receive: Receive = {
    case TokenBasedActor.Token =>
      avalable = (sender(), DateTime.now()) :: avalable
      if (waitingMessages.nonEmpty) {
        trySendBuffer()
      }
    case CleanBuffers =>
      cleanBuffers()
    case msg =>
      avalable match {
        case (a, _) :: rest =>
          avalable = rest
          a.tell(msg, sender())
        case _ =>
          waitingMessages = (msg, sender(), DateTime.now()) :: waitingMessages
      }
      spawnChild()
  }

  private def spawnChild() = {
    if (context.children.size < number) {
      context.actorOf(Props(new TokenBasedActor(child)))
    }
  }
}
