package controllers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import code.AllowedFields
import code.annotation._
import code.transport.lattice.{
  CandidateNode,
  EditableSentence,
  EditableSentencePart,
  PartialAnalysisQuery
}
import com.typesafe.scalalogging.StrictLogging
import javax.inject.Inject
import play.api.mvc.{InjectedController, Result}
import reactivemongo.bson.BSONObjectID
import ws.kotonoha.akane.analyzers.jumanpp.wire.lattice.LatticeDump

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

class SentenceApiController @Inject()(
    dbo: SentenceDbo,
    objLog: LogDbo,
    reportSvc: SentenceReportService,
    jpp: JumanppGrpcService
)(implicit ec: ExecutionContext)
    extends InjectedController
    with StrictLogging {

  def doHandle(body: SentenceRequest, user: AnnotationToolUser): Future[Result] = {
    objLog.log(body, user)

    body.request match {
      case SentenceRequest.Request.Sentences(sents) =>
        dbo
          .getSentences(user, sents)
          .flatMap(sents => jpp.checkTokens(sents))
          .map(sents => Ok(LiftPB(sents)))
      case SentenceRequest.Request.Annotate(obj) =>
        val uid = if (user.admin) {
          BSONObjectID.parse(obj.annotatorId.id).getOrElse(user._id)
        } else user._id
        dbo.annotate(obj, uid).map(x => Ok(LiftPB(x)))
      case SentenceRequest.Request.Merge(req) =>
        val uid = if (user.admin) {
          BSONObjectID.parse(req.annotatorId.id).getOrElse(user._id)
        } else user._id
        if (req.edits.isEmpty) {
          Future.successful(BadRequest("Edits are empty"))
        } else {
          dbo.mergeEdits(uid, req).map(x => Ok(LiftPB(x)))
        }
      case SentenceRequest.Request.Review(req) =>
        logger.debug(req.toString)
        val uid = if (user.admin) {
          BSONObjectID.parse(req.annotatorId.id).getOrElse(user._id)
        } else user._id
        dbo.publishReview(uid, req.sentenceId).map(r => Ok(LiftPB(r)))
      case SentenceRequest.Request.BothBad(req) =>
        val uid = if (user.admin) {
          BSONObjectID.parse(req.annotatorId.id).getOrElse(user._id)
        } else user._id
        dbo.saveBadLog(uid, req).map { _ =>
          Ok(LiftPB(Annotation()))
        }
      case SentenceRequest.Request.RecalcStatuses(_) =>
        if (user.admin) {
          dbo.recalcStatuses().map { _ =>
            Ok(LiftPB(Annotation()))
          }
        } else {
          Future.successful(Forbidden("You are not admin"))
        }
      case _ => Future.successful(NotImplemented)
    }
  }

  def handler() = Action.async(LiftPB.protoBodyParser[SentenceRequest]) { implicit req =>
    req.attrs.get(SessionUser.User) match {
      case Some(u) => doHandle(req.body, u)
      case None    => Future.successful(Unauthorized("You are not allowed here"))
    }
  }

  def sentence(id: String) = Action.async { implicit req =>
    dbo.findById(id).map {
      case None    => NotFound(s"No sentence with id: $id")
      case Some(s) => Ok(LiftPB(s))
    }
  }

  def reportAnalysis() = Action.async(LiftPB.protoBodyParser[ReportInvalidResult]) { implicit req =>
    val f = req.attrs.get(SessionUser.User) match {
      case Some(u) => reportSvc.handleSentenceReport(req.body, u.name)
      case None    => reportSvc.handleSentenceReport(req.body, "anonymous")
    }
    f.map(Ok(_))
  }
}

class BlockSeqBuilder(allowedFields: AllowedFields) {
  private var blockStart = 0
  private var blockEnd = 0
  private var nodeStart = 0
  private var partStart = 0

  private val nodes = new ArrayBuffer[CandidateNode]
  private val parts = new ArrayBuffer[EditableSentencePart]
  private var prevComplex = false
  private val blocks = new ArrayBuffer[SentenceBlock]()

  private def addNode(node: CandidateNode) = {
    if (nodes.isEmpty || nodes.last.ne(node)) {
      nodes += node
    }
  }

  private def addPart(part: EditableSentencePart) = {
    if (parts.isEmpty || parts.last.ne(part)) {
      parts += part
    }
  }

  private def createPartSpan(): TokenSpan = {
    var start = partStart
    val iter = parts.iterator

    val result = new ArrayBuffer[ExampleToken]()

    while (iter.hasNext) {
      val x = iter.next()
      val end = start + x.surface.length

      if (start >= blockStart && end <= blockEnd) {
        result += ExampleToken(
          surface = x.surface,
          unit = x.tags.nonEmpty,
          tags = x.tags
            .filter(t => allowedFields.isAllowed(t.key) && t.value != x.surface)
            .map(t => t.key -> t.value)
            .toMap
        )
      } else {
        val sstart = Math.max(0, blockStart - start)
        val send = x.surface.length - Math.max(0, end - blockEnd)

        val surf = x.surface.substring(sstart, send)

        result += ExampleToken(
          surface = surf
        )
      }

      start = end
    }

    if (start > blockEnd) {
      val last = parts.last
      assert(last.tags.isEmpty)
      partStart = start - last.surface.length
      parts.clear()
      parts += last
    } else {
      parts.clear()
      partStart = start
    }

    TokenSpan(
      index = 0,
      tokens = result
    )
  }

  private def createNodeSpan(): TokenSpan = {
    var start = nodeStart
    val iter = nodes.iterator

    val result = new ArrayBuffer[ExampleToken]()
    while (iter.hasNext) {
      val x = iter.next()
      val end = start + x.surface.length

      result += ExampleToken(
        surface = x.surface,
        unit = x.tags.nonEmpty,
        tags = x.tags
          .filter(t => allowedFields.isAllowed(t.key) && t.value != x.surface)
          .map(t => t.key -> t.value)
          .toMap
      )

      assert(end <= blockEnd)
      start = end
    }

    assert(start == blockEnd)

    nodes.clear()

    TokenSpan(
      index = 1,
      tokens = result
    )
  }

  private def createBlock(): Unit = {
    if (nodes.isEmpty && parts.isEmpty) {
      return
    }

    if (prevComplex) {
      val left = createPartSpan()
      val right = createNodeSpan()
      blocks += SentenceBlock(
        offset = blockStart,
        spans = Seq(left, right)
      )
    } else if (parts.nonEmpty) {
      val left = createPartSpan()
      blocks += SentenceBlock(
        offset = blockStart,
        spans = Seq(left)
      )
    }
  }

  private def handlePair(
      node: CandidateNode,
      part: EditableSentencePart,
      ns: Int,
      ne: Int,
      ps: Int,
      pe: Int,
      start: Int,
      end: Int): Unit = {
    val complex = part.tags.nonEmpty || (prevComplex && ns < ps)
    (prevComplex, complex) match {
      case (false, false) =>
        addPart(part)
      case (false, true) =>
        blockEnd = Math.min(start, ns)
        createBlock()
        blockStart = blockEnd
        nodeStart = ns
        addNode(node)
        addPart(part)
      case (true, false) =>
        blockEnd = start
        createBlock()
        blockStart = start
        nodeStart = ns
        addPart(part)
      case (true, true) =>
        addNode(node)
        addPart(part)
    }
    prevComplex = complex
  }

  def make(top1: Seq[CandidateNode], parts: Seq[EditableSentencePart]): Unit = {
    val i1 = top1.iterator.buffered
    val i2 = parts.iterator.buffered

    var s1 = 0
    var s2 = 0
    var start = 0

    while (i1.hasNext || i2.hasNext) {
      val x1 = i1.head
      val x2 = i2.head

      val e1 = s1 + x1.surface.length
      val e2 = s2 + x2.surface.length

      if (e1 == e2) {
        handlePair(x1, x2, s1, e1, s2, e2, start, e1)
        i1.next()
        i2.next()
        s1 = e1
        s2 = e2
        start = e1
      } else if (e1 > e2) {
        handlePair(x1, x2, s1, e1, s2, e2, start, e2)
        i2.next()
        s2 = e2
        start = e2
      } else {
        handlePair(x1, x2, s1, e1, s2, e2, start, e1)
        i1.next()
        s1 = e1
        start = e1
      }
    }
    blockEnd = start
    createBlock()
  }

  def result(): Seq[SentenceBlock] = blocks
}

class SentenceReportService @Inject()(
    dbo: SentenceDbo,
    jumanpp: JumanppGrpcService,
    allowedFields: AllowedFields
)(implicit ec: ExecutionContext)
    extends StrictLogging {

  def handleMerge(dump: LatticeDump, sent: EditableSentence, reporter: String): Future[String] = {
    val top1 = LatticeUtil.top1(dump)
    val bsb = new BlockSeqBuilder(allowedFields)
    bsb.make(top1, sent.parts)
    val blocks = bsb.result()
    val hasUnit = blocks.exists { sb =>
      if (sb.spans.length == 2) {
        val left = sb.spans(0)
        val right = sb.spans(1)
        left.tokens.exists(_.unit) && right.tokens.exists(_.unit)
      } else false
    }

    if (!hasUnit) {
      return Future.successful("no units")
    }

    val surfaceComment = top1.map(_.surface).mkString("# ", "", "")

    dbo.findAlreadyReported(surfaceComment).flatMap {
      case Some(id) =>
        logger.debug(s"sentence $surfaceComment is already present with id: $id")
        Future.successful(id)
      case None =>
        val hashCode = sent.hashCode().toHexString
        val fmt = DateTimeFormatter.ofPattern("uuuuMMddHHmmss")
        val id = s"user-${fmt.format(LocalDateTime.now())}-$hashCode"

        val sentence = Sentence(
          id = id,
          blocks = blocks,
          originalComments = Seq(
            surfaceComment
          ),
          tags = Seq(
            "report",
            s"@$reporter"
          ),
          importedOn = Some(Timestamps.now)
        )
        dbo.saveSentences(Seq(sentence)).map(_ => id)
    }
  }

  def handleSentenceReport(input: ReportInvalidResult, reporter: String): Future[String] = {
    val so = input.sentence
    if (so.isEmpty) {
      return Future.successful("input=None")
    }

    val sent = so.get
    if (!sent.parts.exists(_.node)) {
      return Future.successful("empty query")
    }

    val plainQuery = PartialAnalysisQuery(
      input = Some(
        EditableSentence(
          parts = EditableSentencePart(
            surface = sent.parts.map(_.surface).mkString
          ) :: Nil
        )
      ))

    jumanpp.makeRequest(plainQuery) match {
      case None      => Future.successful("empty request")
      case Some(req) => jumanpp.analyze(req).flatMap(dump => handleMerge(dump, sent, reporter))
    }
  }

}
