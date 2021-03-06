package code.annotation

import java.io.InputStream

import code.transport.lattice.{LatticeSubset, PartialAnalysisQuery}
import org.scalajs.dom.crypto.BufferSource
import org.scalajs.dom.experimental._
import org.scalajs.dom.WebSocket

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.{Dictionary, URIUtils}
import scala.scalajs.js.typedarray.Int8Array
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

class JsTypedArrayInputStream(buf: Int8Array) extends InputStream {
  private var position = 0

  override def read(): Int = {
    if (position >= buf.length) {
      return -1
    }
    val ret = buf.get(position) & 0xff
    position += 1
    ret
  }

  override def read(b: Array[Byte]): Int = read(b, 0, b.length)

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    if (available() == 0) {
      return -1
    }

    var pos = position
    var idx = off
    val maxRead = len - off
    val end = maxRead min (buf.length - pos)
    var numRead = 0
    while (numRead < end) {
      b(idx) = buf.get(pos)
      idx += 1
      pos += 1
      numRead += 1
    }
    position = pos

    numRead
  }

  override def available(): Int = buf.length - position

  override def skip(n: Long): Long = {
    val pos = position + n.toInt
    val actual = pos min buf.length
    val prev = position
    position = actual
    actual - prev
  }

  override def reset(): Unit = {
    position = 0
  }
}

case class WebsocketConnection[Fn](fn: Fn, socket: WebSocket)

object ApiService {
  implicit class RichFutureResponce(val fut: Future[Response]) extends AnyVal {
    @inline
    def as[T <: GeneratedMessage with Message[T]](
        implicit comp: GeneratedMessageCompanion[T],
        ec: ExecutionContext): Future[T] = {
      fut.flatMap { resp =>
        if (resp.ok && resp.headers.get("Content-Type").get == "application/x-protobuf") {
          resp.arrayBuffer().toFuture.map { ab =>
            val buf = new Int8Array(ab)
            comp.parseFrom(new JsTypedArrayInputStream(buf))
          }
        } else {
          Future.failed(new RuntimeException("request failed"))
        }
      }
    }
  }
}

class ApiService(apiUrl: String, csrfToken: String)(implicit ec: ExecutionContext) {
  import ApiService._

  def postProtobuf(requestPath: String, msg: GeneratedMessage): Future[Response] = {
    import scala.scalajs.js.typedarray._
    val bytes = msg.toByteArray
    val u8arr = bytes.toTypedArray
    val src: BufferSource = u8arr
    Fetch
      .fetch(
        requestPath,
        RequestInit(
          credentials = RequestCredentials.include,
          method = HttpMethod.POST,
          headers = Dictionary.apply(
            "Content-Type" -> "application/x-protobuf",
            "Csrf-Token" -> csrfToken
          ),
          body = src
        )
      )
      .toFuture
  }

  def postProtobufAs[T <: GeneratedMessage with Message[T]](
      requestPath: String,
      msg: GeneratedMessage)(implicit comp: GeneratedMessageCompanion[T]): Future[T] = {
    postProtobuf(requestPath, msg).as[T]
  }

  def userListCommand(cmd: AnnotationUserCommand): Future[Seq[AnnotationUser]] = {
    val requestPath = s"$apiUrl/users"
    postProtobuf(requestPath, cmd).flatMap { resp =>
      if (resp.ok && resp.headers.get("Content-Type").get == "application/x-protobuf") {
        resp.arrayBuffer().toFuture.map { ab =>
          val buf = new Int8Array(ab)
          AllUsers.parseFrom(new JsTypedArrayInputStream(buf)).users
        }
      } else {
        Future.failed(new RuntimeException("request failed"))
      }
    }
  }

  def sentenceCall[R <: GeneratedMessage with Message[R]](cmd: SentenceRequest)(
      implicit comp: GeneratedMessageCompanion[R]): Future[R] = {
    val requestPath = s"$apiUrl/sentences"
    postProtobuf(requestPath, cmd).as[R]
  }

  def user(): Future[AnnotationUser] = {
    val requestPath = s"$apiUrl/user"
    Fetch
      .fetch(requestPath, RequestInit(credentials = RequestCredentials.include))
      .toFuture
      .flatMap { resp =>
        resp.arrayBuffer().toFuture.map { ab =>
          val buf = new Int8Array(ab)
          AnnotationUser.parseFrom(new JsTypedArrayInputStream(buf))
        }
      }
  }

  def updateUser(user: AnnotationUser): Future[AnnotationUser] = {
    val requestPath = s"$apiUrl/user"
    postProtobuf(requestPath, user).as[AnnotationUser]
  }

  def partialQuery(q: PartialAnalysisQuery): Future[LatticeSubset] = {
    val requestPath = s"$apiUrl/pana"
    postProtobuf(requestPath, q).as[LatticeSubset]
  }

  def importWsCall(onMessage: String => Unit): WebSocket = {
    val wsUrl = apiUrl.replaceFirst("http", "ws")
    var fullUrl = s"$wsUrl/importws"
    if ("wss?://".r.findFirstMatchIn(fullUrl).isEmpty) {
      val loc = org.scalajs.dom.document.location
      val baseProtocol = loc.protocol
      val prot = baseProtocol match {
        case "http:"  => "ws"
        case "https:" => "wss"
        case _        => throw new Exception(s"Invalid protocol: $baseProtocol")
      }
      fullUrl = s"$prot://${loc.host}$fullUrl"
    }
    val socket = new WebSocket(fullUrl)
    socket.onmessage = { ev =>
      onMessage(ev.data.toString)
    }
    socket
  }

  def sentenceById(id: String): Future[Option[Sentence]] = {
    val encoded = URIUtils.encodeURIComponent(id)
    val uri = s"$apiUrl/sentence/$encoded"
    Fetch.fetch(uri).toFuture.flatMap { resp =>
      if (resp.status == 404) {
        Future.successful(None)
      } else {
        resp.arrayBuffer().toFuture.map { ab =>
          val buf = new Int8Array(ab)
          Some(Sentence.parseFrom(new JsTypedArrayInputStream(buf)))
        }
      }
    }
  }

  def reportAnalysisResult(msg: ReportInvalidResult): Future[String] = {
    postProtobuf(s"$apiUrl/report", msg).flatMap { resp =>
      resp.text().toFuture
    }
  }

}
