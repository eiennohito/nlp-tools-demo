package controllers

import java.util.concurrent.Callable

import code._
import com.google.common.cache.{Cache, CacheBuilder}
import com.google.inject.Inject
import com.typesafe.scalalogging.StrictLogging
import play.api.mvc.{Action, Controller}
import ws.kotonoha.akane.analyzers.jumanpp.wire.LatticeNode.LatticeNodeLens
import ws.kotonoha.akane.analyzers.jumanpp.wire.{Lattice, LatticeNode}
import ws.kotonoha.akane.parser.JumanPosSet

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author eiennohito
  * @since 2016/09/28
  */
class Jumanpp @Inject() (
  jc: JumanppCache
)(implicit ec: ExecutionContext) extends Controller {
  def lattice() = Action.async { req =>
    val graph = req.getQueryString("text") match {
      case None => Future.successful("")
      case Some(sent) => jc.get(sent.take(512)).map(LatticeFormatter.formatLatticeAsGraph)
    }
    graph.map(gs => Ok(views.html.lattice(req.getQueryString("text").getOrElse(""), gs)))
  }

  def lattice2() = Action { req =>
    val query = req.getQueryString("text").getOrElse("")
    Ok(views.html.lattice2(query, jc.version))
  }

  def lattice_parse() = Action.async { req =>
    req.getQueryString("text") match {
      case None => Future.successful(Ok("null"))
      case Some(txt) =>
        val part = txt.take(512)
        jc.get(part).map { l =>
          val posset = JumanPosSet.default

          val tansformed = JumanppLattice(l.nodes.map { n =>
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
          val string = upickle.default.write(tansformed)
          Ok(string)
        }
    }
  }
}

object LatticeFormatter {
  def formatLatticeAsGraph(l: Lattice): String = {
    val byId = l.nodes.map(n => n.nodeId -> n).toMap

    val bldr = new StringBuilder
    bldr.append("graph LR;\n")
    for (n <- l.nodes) {
      val nranks = ranks(n)
      for (prev <- n.prevNodes) {
        val pn = byId.get(prev)
        val prevRanks = pn.map(n => ranks(n)).getOrElse(Array.emptyIntArray)
        val sameRanks = prevRanks.intersect(nranks)
        bldr.append(s"N$prev -${formatRanks(sameRanks)}-> N${n.nodeId}").append(";\n")
      }
      bldr.append(s"""N${n.nodeId}["${n.surface}"];""").append("\n")
      bldr.append(s"class N${n.nodeId} simple;\n")
    }
    bldr.append("N0(BOS);\n")
    bldr.append("class N0 simple;\n")
    bldr.result()
  }

  private def formatRanks(common: Array[Int]): String = {
    if (common.isEmpty) ""
    else common.mkString("|", ",", "|")
  }

  private def ranks(pn: LatticeNode) = {
    pn.features
      .find(_.key == "ランク")
      .flatMap(_.value)
      .map(_.split(",").map(_.toInt))
      .getOrElse(Array.emptyIntArray)
  }
}

class JumanppCache @Inject() (
  jpp: JumanppService
)(implicit ec: ExecutionContext) extends StrictLogging {
  def version: String = jpp.version

  private val cache = CacheBuilder.newBuilder().maximumSize(10000).build[String, Future[Lattice]]()

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
