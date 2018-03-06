package controllers

import javax.inject.Inject

import code.annotation.{AnnotationAuth, ExampleToken, Sentence, SentenceDbo}
import com.typesafe.scalalogging.StrictLogging
import play.api.mvc.{InjectedController, QueryStringBindable}
import ws.kotonoha.akane.utils.XInt

import scala.concurrent.{ExecutionContext, Future}

case class DownloadArgs(token: String, query: String)

object DownloadArgs extends StrictLogging {
  implicit def downloadArgsBindable(
      implicit sb: QueryStringBindable[String]): QueryStringBindable[DownloadArgs] =
    new QueryStringBindable[DownloadArgs] {
      override def bind(
          key: String,
          params: Map[String, Seq[String]]): Option[Either[String, DownloadArgs]] = {
        for {
          token <- sb.bind("token", params)
          query <- sb.bind("query", params)
        } yield {
          (token, query) match {
            case (Right(t), Right(q)) => Right(DownloadArgs(t, q))
            case _                    => Left("Failed to get query parameters for download args")
          }
        }
      }

      override def unbind(key: String, value: DownloadArgs): String = {
        s"${sb.unbind("token", value.token)}&${sb.unbind("query", value.query)}"
      }
    }
}

class SentenceExport @Inject()(
    users: AnnotationAuth,
    sents: SentenceDbo
)(implicit ec: ExecutionContext)
    extends InjectedController {

  def download(args: DownloadArgs) = Action.async { implicit req =>
    users.checkToken(args.token).map {
      case None => Unauthorized("Goodbye")
      case Some(u) =>
        val sentences = sents.stream(u, args.query)
        Ok.chunked(sentences.map(SentenceExport.exportSentence))
    }
  }

}

object SentenceExport {
  private def writeToken(token: ExampleToken, builder: StringBuilder): Unit = {
    if (token.unit) {
      builder.append('\t')
    }
    builder.append(token.surface)
    if (token.unit && token.tags.nonEmpty) {
      for ((k, v) <- token.tags) {
        builder.append('\t').append(k).append(':').append(v)
      }
    }
    builder.append('\n')
  }

  def exportSentence(s: Sentence): String = {
    val sb = new StringBuilder
    sb.append("# S-ID:")
    sb.append(s.id)
    sb.append('\n')
    var bad = false

    for (block <- s.blocks) {
      val anns = block.annotations.map(_.value).distinct
      if (anns.isEmpty && block.spans.lengthCompare(1) == 0) {
        block.spans.head.tokens.foreach(t => writeToken(t, sb))
      } else if (anns.lengthCompare(1) == 0) {
        val annValue = anns.head
        annValue match {
          case XInt(i) if block.spans.lengthCompare(i) > 0 =>
            val sp = block.spans(i)
            sp.tokens.foreach(t => writeToken(t, sb))
          case _ =>
            bad = true
        }
      } else {
        bad = true
      }
    }
    sb.append('\n')
    if (bad) {
      s"# S-ID:${s.id}\nBAD\n\n"
    } else {
      sb.result()
    }
  }
}
