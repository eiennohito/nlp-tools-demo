package controllers

import java.util.Locale

import code.grpc.LatticeDumpJppGrpcAnalyzer
import code.transport.lattice._
import com.ibm.icu.text.BreakIterator
import com.typesafe.scalalogging.StrictLogging
import javax.inject.{Inject, Provider}
import play.api.mvc.InjectedController
import ws.kotonoha.akane.analyzers.jumanpp.grpc.{AnalysisRequest, JumanppConfig, RequestType}
import ws.kotonoha.akane.analyzers.jumanpp.wire.lattice.{FieldValue, LatticeDump, LatticeNode}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

object AllowedFields {
  //TODO: fix with something better
  val allowedFields = Set(
    "surface",
    "baseform",
    "pos",
    "subpos",
    "conjform",
    "conjtype"
  )
}

class JumanppGrpcService @Inject()(
    ana: Provider[LatticeDumpJppGrpcAnalyzer]
)(implicit ec: ExecutionContext) {

  private def makeRequestImpl(sentence: EditableSentence): AnalysisRequest = {
    val sb = new StringBuilder
    for (line <- sentence.parts) {
      if (line.node) {
        sb.append('\t').append(line.surface)
        for (tag <- line.tags.filter(t => AllowedFields.allowedFields.contains(t.key))) {
          sb.append('\t').append(tag.key).append(':').append(tag.value)
        }
      } else {
        sb.append(line.surface)
      }
      sb.append('\n')
    }
    AnalysisRequest(
      sentence = sb.result(),
      `type` = RequestType.PartialAnnotation,
      config = Some(
        JumanppConfig( // full beam analysis w/o RNN
                      globalBeamLeft = -1,
                      globalBeamCheck = -1,
                      ignoreRnn = true))
    )
  }

  def makeRequest(query: PartialAnalysisQuery): Option[AnalysisRequest] = {
    query.input match {
      case None                                             => None
      case Some(q) if q == EditableSentence.defaultInstance => None
      case Some(q) if q.parts.forall(_.surface.isEmpty)     => None
      case Some(i)                                          => Some(makeRequestImpl(i))
    }
  }

  def analyze(in: AnalysisRequest): Future[LatticeDump] = {
    ana.get().analyze(in)
  }
}

class JumanppGrpc @Inject()(
    svc: JumanppGrpcService
)(implicit ec: ExecutionContext)
    extends InjectedController
    with StrictLogging {

  def analyze() = Action.async { req =>
    val q = req.getQueryString("s").getOrElse("")

    val resf = if (q.length == 0) {
      Future.successful(LatticeDump.defaultInstance)
    } else {
      val req = AnalysisRequest(sentence = q)
      svc.analyze(req)
    }

    resf.map(d => Ok(views.html.ldump(d)))
  }

  def partialAnalysisApi() = Action.async(LiftPB.protoBodyParser[PartialAnalysisQuery]) { req =>
    svc.makeRequest(req.body) match {
      case None => Future.successful(Ok(LiftPB(LatticeSubset.defaultInstance)))
      case Some(areq) =>
        svc.analyze(areq).map(x => Ok(LiftPB(LatticeUtil.subset(x, req.body))))
    }
  }
}

object LatticeUtil {
  def computeTags(node: LatticeNode, names: Seq[String]): Seq[NodeTag] = {
    node.values.zip(names).flatMap {
      case (v, name) =>
        v.value match {
          case FieldValue.Value.String(s) if s.length > 0 => NodeTag(name, s) :: Nil
          case _                                          => Nil
        }
    }
  }

  def top1(dump: LatticeDump): Seq[CandidateNode] = {
    val bnds = dump.boundaries
    val eos = bnds.last.nodes.head
    val top1Beam = eos.beams.head
    var prev = top1Beam.nodes(1)
    var topNodes: List[CandidateNode] = Nil

    // build a list from the end,
    // and we get the correct order automatically
    while (prev.boundary >= 2) {
      val node = bnds(prev.boundary - 2).nodes(prev.node)
      val beam = node.beams(prev.beam)

      val theNode = CandidateNode(
        id = prev.boundary * 100000 + prev.node,
        surface = node.surface,
        tags = computeTags(node, dump.fieldNames),
        score = beam.cumScore
      )

      topNodes = theNode :: topNodes

      prev = beam.nodes(1)
    }

    topNodes
  }

  def subset(dump: LatticeDump, query: PartialAnalysisQuery): LatticeSubset = {
    val focused = query.focus match {
      case None => Nil
      case Some(f) =>
        if (f.end == 0) {
          computeNodesSpanning(dump, f.start)
        } else {
          computeNodesSpanning(dump, f.start, f.end)
        }
    }

    LatticeSubset(
      top = LatticeUtil.top1(dump),
      focusNodes = focused,
      graphemes = graphems(query.input.toSeq.flatMap(_.parts.map(_.surface)).mkString)
    )
  }

  private def computeNodesSpanning(dump: LatticeDump, position: Int) = {
    val codePtIdx = dump.surface.codePointCount(0, position)
    val result = new ArrayBuffer[CandidateNode]()

    for {
      (bnd, bndId2) <- dump.boundaries.zipWithIndex
      (node, nodeId) <- bnd.nodes.zipWithIndex
    } {
      val codeptOffset = dump.surface.offsetByCodePoints(0, bndId2)
      val nodeEnd = bndId2 + node.length
      if (codePtIdx >= bndId2 && codePtIdx < nodeEnd && node.length > 0) {
        val score = node.beams.map(_.cumScore).max

        val theId = (bndId2 + 2) * 100000 + nodeId
        result += CandidateNode(
          id = theId,
          surface = node.surface,
          tags = LatticeUtil.computeTags(node, dump.fieldNames),
          score = score,
          start = codeptOffset
        )
      }
    }

    result.sortBy(-_.score)
  }

  private def computeNodesSpanning(dump: LatticeDump, start: Int, end: Int) = {
    val codePtIdx = dump.surface.codePointCount(0, start)
    val codePtLength = dump.surface.codePointCount(start, end)
    val result = new ArrayBuffer[CandidateNode]()

    val nodes = dump.boundaries(codePtIdx).nodes

    for {
      (node, nodeId) <- nodes.zipWithIndex
    } {
      if (node.length == codePtLength) {
        val score = node.beams.map(_.cumScore).max
        val theId = (codePtIdx + 2) * 100000 + nodeId

        result += CandidateNode(
          id = theId,
          surface = node.surface,
          tags = LatticeUtil.computeTags(node, dump.fieldNames),
          score = score,
          start = start
        )
      }
    }

    result.sortBy(-_.score)
  }

  private def graphems(input: String): Seq[String] = {
    val iter = BreakIterator.getCharacterInstance(Locale.JAPANESE)
    iter.setText(input)

    var start = 0 // WILL return 0 on first call
    var pos = BreakIterator.DONE
    val seq = new ArrayBuffer[String]()

    while ({
      pos = iter.next()
      pos != BreakIterator.DONE
    }) {
      seq += input.substring(start, pos)
      start = pos
    }
    seq
  }
}
