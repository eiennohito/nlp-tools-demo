package controllers

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import javax.inject.Inject

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.Materializer
import code.annotation._
import com.typesafe.scalalogging.StrictLogging
import play.api.libs.streams.ActorFlow
import play.api.mvc.{InjectedController, WebSocket}
import play.filters.csrf.CSRF
import ws.kotonoha.akane.io.Charsets

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

class SentenceImportController @Inject()(
    sentences: SentenceDbo
)(implicit ec: ExecutionContext, adminAction: AdminAction, sys: ActorSystem, mat: Materializer)
    extends InjectedController
    with StrictLogging {

  def importFilesystem() = WebSocket.acceptOrResult[String, String] { implicit req =>
    //val csrfToken = CSRF.getToken.getOrElse(throw new Exception("Should have CSRF token here"))
    SessionUser.getUser(req) match {
      case Some(u) if u.admin =>
        val flow = ActorFlow.actorRef { out =>
          Props(new ImportSocketActor(out, sentences))
        }
        Future.successful(Right(flow))
      case _ =>
        Future.successful(Left(Unauthorized("Not allowed here!")))
    }
  }
}

class ImportSocketActor(out: ActorRef, db: SentenceDbo) extends Actor with StrictLogging {
  private def info(str: String): Unit = {
    logger.info(str)
    out ! str
  }

  import ws.kotonoha.akane.resources.FSPaths._
  private def importFrom(p: Path, tags: Seq[String]): Unit = {
    try {
      val is = Files.newInputStream(p, StandardOpenOption.READ)
      val isr = new InputStreamReader(is, Charsets.utf8)
      val br = new BufferedReader(isr)
      self ! ImportSocketActor.Read(br, tags)
    } catch {
      case e: Exception =>
        info(s"failed to read from: $p")
        logger.error(s"failed to read from $p", e)
    }
  }

  private def readItems(rdr: BufferedReader, num: Int): Seq[Sentence] = {
    0.until(num).flatMap { _ =>
      JumanppDiffReader.readExample(rdr).get
    }
  }

  def readFrom(rdr: BufferedReader, tags: Seq[String]): Unit = {
    val atTime = 50
    val items = try { readItems(rdr, atTime) } catch {
      case e: Exception =>
        logger.error("Failed to read items")
        Nil
    }

    val ids = items.map(_.id)

    implicit val ec: ExecutionContextExecutor = context.dispatcher

    db.checkExistance(ids)
      .flatMap { existing =>
        for (e <- existing) {
          info(s"ignoring S-ID:$e")
        }
        val nowTime = Some(Timestamps.now)
        val newItems = items
          .filterNot(item => existing.contains(item.id))
          .map(
            s => s.copy(importedOn = nowTime, tags = tags)
          )
        db.saveSentences(newItems)
      }
      .onComplete {
        case scala.util.Success(inserted) =>
          info(s"saved $inserted examples")
          if (items.lengthCompare(atTime) < 0) {
            rdr.close()
            info("finished!")
          } else {
            self ! ImportSocketActor.Read(rdr, tags)
          }
        case scala.util.Failure(e) =>
          rdr.close()
          info("failed to save")
      }
  }

  override def receive: Receive = {
    case input: String =>
      val parts = input.split(",").map(_.trim)
      val s = parts.head
      val tags = parts.tail.toSeq

      val p = Paths.get(s)
      if (!Files.isReadable(p)) {
        info(s"Can't import from $p")
      } else {
        info(s"Starting to import files from $p")
        try {
          importFrom(p, tags)
        } catch {
          case e: Exception =>
            info(s"failed to import from: $p")
            info(e.toString)
            logger.error(s"failed to import from $p", e)
        }
      }
    case ImportSocketActor.Read(rdr, tags) =>
      readFrom(rdr, tags)
  }
}

object ImportSocketActor {
  case class Read(rdr: BufferedReader, tags: Seq[String])
}
