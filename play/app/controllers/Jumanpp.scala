package controllers

import java.util.concurrent.{Callable, TimeUnit}

import code._
import com.google.common.cache.CacheBuilder
import com.google.inject.Inject
import com.typesafe.scalalogging.StrictLogging
import org.joda.time.{DateTime, DateTimeZone}
import play.api.libs.json.Json
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import ws.kotonoha.akane.analyzers.jumanpp.wire.Lattice
import ws.kotonoha.akane.parser.JumanPosSet

import scala.concurrent.{ExecutionContext, Future}


case class ReportForAnalyze(
  id: String,
  ids: Seq[Int]
)

object ReportForAnalyze {
  implicit val format = Json.format[ReportForAnalyze]
}

/**
  * @author eiennohito
  * @since 2016/09/28
  */
class Jumanpp @Inject() (
  jc: JumanppCache,
  mwork: MongoWorker
)(implicit ec: ExecutionContext) extends Controller with StrictLogging {

  def lattice() = Action { req =>
    val query = req.getQueryString("text").getOrElse("")
    Ok(views.html.lattice(query, jc.version, jc.dicVersion))
  }

  def log(id: BSONObjectID, l: Lattice, txt: String, req: Request[AnyContent], nanoStart: Long): Unit = {
    val millis = (System.nanoTime() - nanoStart) * 1e-6
    val ip = req.headers.get("X-Real-IP").getOrElse(req.remoteAddress)
    val ua = req.headers.get("User-Agent")

    val anal = JppAnalysis(
      id,
      DateTime.now(DateTimeZone.UTC),
      txt,
      l,
      jc.version,
      jc.dicVersion,
      ip,
      ua.getOrElse("empty"),
      millis,
      None
    )

    mwork.save(anal).onFailure {
      case e: Exception => logger.warn(s"could not save analysis results for input: $txt", e)
    }
  }

  def lattice_parse() = Action.async { req =>
    req.getQueryString("text") match {
      case None => Future.successful(Ok("null"))
      case Some(txt) =>
        val part = txt.take(512)
        val latF = jc.get(part)
        val start = System.nanoTime()
        val id = reactivemongo.bson.generateId
        latF.foreach(l => log(id, l, txt, req, start))
        latF.map { l =>
          val transformed = JumanppConversion.convertLatttice(id, l)
          val string = upickle.default.write(transformed)
          Ok(string)
        }
    }
  }

  def report() = Action.async(parse.json[ReportForAnalyze]) { req =>
    val obj = req.body
    mwork.updateReport(obj.id, obj.ids).map { _ => Ok("Ok") }
  }
}

object JumanppConversion {
  val posset = JumanPosSet.default

  def convertLatttice(id: BSONObjectID, l: Lattice) = {
    JumanppLattice(
      id.stringify,
      l.nodes.map { n =>
        val po = n.pos
        val pos = posset.pos(po.pos)
        val subpos = pos.subtypes(po.subpos)
        val ctype = posset.conjugatons(po.category)
        val conj = ctype.conjugations(po.conjugation)
        val features = n.features.map {jo => JumanppFeature(jo.key, jo.value)}
        JumanppLatticeNode(
          n.nodeId,
          n.startIndex,
          n.surface,
          n.canonic,
          n.reading,
          n.midasi,
          pos.name, subpos.name, ctype.name, conj.name,
          po.pos, po.subpos, po.category, po.conjugation,
          n.prevNodes,
          n.rank,
          features,
          NodeScores(
            n.featureScore,
            n.lmScore,
            n.morphAnalysisScore
          )
        )
      }, l.comment.getOrElse(""))
  }
}


class JumanppCache @Inject() (
  jpp: JumanppService
)(implicit ec: ExecutionContext) extends StrictLogging {
  def dicVersion: String = jpp.dicVersion
  def version: String = jpp.version

  private val cache = CacheBuilder.newBuilder()
    .maximumSize(10000)
    .expireAfterWrite(1, TimeUnit.DAYS)
    .build[String, Future[Lattice]]()

  def get(s: String): Future[Lattice] = {
    cache.get(s, new Callable[Future[Lattice]] {
      override def call() = {
        val item = jpp.analyze(s)
        item.onFailure {
          case e: Exception =>
            logger.error(s"could not create value for $s", e)
            cache.invalidate(s)
        }
        item
      }
    })
  }
}
