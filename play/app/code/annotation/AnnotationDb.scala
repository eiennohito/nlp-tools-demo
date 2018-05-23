package code.annotation

import akka.NotUsed
import akka.stream.scaladsl.Source
import com.google.inject.Provides
import com.google.protobuf.timestamp.Timestamp
import com.typesafe.scalalogging.StrictLogging
import controllers.AllowedFields
import javax.inject.{Inject, Singleton}
import net.codingwell.scalaguice.ScalaModule
import org.apache.commons.lang3.RandomUtils
import org.joda.time.DateTime
import play.api.Configuration
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.UpdateWriteResult
import reactivemongo.api.{Cursor, DefaultDB, MongoConnection}
import reactivemongo.bson.Macros.Annotations.Key
import reactivemongo.bson.{
  BSON,
  BSONArray,
  BSONDateTime,
  BSONDocument,
  BSONDocumentHandler,
  BSONDouble,
  BSONElement,
  BSONHandler,
  BSONInteger,
  BSONObjectID,
  BSONRegex,
  BSONValue,
  Macros
}
import scalapb.{GeneratedEnum, GeneratedEnumCompanion}
import ws.kotonoha.akane.utils.XInt

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

case class AnnotationDb(db: DefaultDB)

object AnnotationDb {
  def createDefaultUser(db: AnnotationDb, username: String, token: String)(
      implicit ec: ExecutionContext): Unit = {
    val aa = new AnnotationAuth(db)

    aa.findAnyAdmin()
      .flatMap {
        case u @ Some(_) => Future.successful(u)
        case None        => aa.checkToken(token)
      }
      .map {
        case Some(u) => //ignore
        case None =>
          val user = AnnotationToolUser(
            _id = BSONObjectID.generate(),
            token = token,
            name = username,
            admin = true
          )
          aa.save(user)
      }

  }
}

class AnnotationDbModule extends ScalaModule {
  import scala.concurrent.duration._

  override def configure(): Unit = {}

  @Provides
  @Singleton
  def annotationDb(mc: MongoConnection, config: Configuration)(
      implicit ec: ExecutionContext): AnnotationDb = {
    val dbname = config.getOptional[String]("annotations.db").getOrElse("annotations")
    val dbf = mc.database(dbname)

    val defaultToken = config.getOptional[String]("annotations.default.token").getOrElse("000000")
    val defaultUsername =
      config.getOptional[String]("annotations.default.username").getOrElse("admin")

    dbf.foreach { db =>
      AnnotationDb.createDefaultUser(AnnotationDb(db), defaultUsername, defaultToken)
    }

    AnnotationDb(Await.result(dbf, 5.seconds))
  }

  @Provides
  def annotationAuth(db: AnnotationDb)(implicit ec: ExecutionContext): AnnotationAuth = {
    new AnnotationAuth(db)
  }
}

object BsonPicklers {
  import prickle._
  implicit val oidPickler: Pickler[BSONObjectID] = new Pickler[BSONObjectID] {
    override def pickle[P](obj: BSONObjectID, state: PickleState)(
        implicit config: PConfig[P]): P = {
      config.makeString(obj.stringify)
    }
  }

  implicit val oidUnpickler: Unpickler[BSONObjectID] = new Unpickler[BSONObjectID] {
    override def unpickle[P](pickle: P, state: mutable.Map[String, Any])(
        implicit config: PConfig[P]): Try[BSONObjectID] = {
      config.readString(pickle).flatMap(BSONObjectID.parse)
    }
  }
}

object AnnotationToolUser {
  implicit val bsonRw: BSONDocumentHandler[AnnotationToolUser] = Macros.handler[AnnotationToolUser]
  import BsonPicklers._
  import prickle._

  implicit val pickler: Pickler[AnnotationToolUser] =
    prickle.Pickler.materializePickler[AnnotationToolUser]
  implicit val unpickler: Unpickler[AnnotationToolUser] =
    prickle.Unpickler.materializeUnpickler[AnnotationToolUser]
}

case class AnnotationToolUser(
    _id: BSONObjectID,
    token: String,
    admin: Boolean,
    name: String
)

class AnnotationAuth(db: AnnotationDb)(implicit ec: ExecutionContext) {

  private val coll = db.db.collection[BSONCollection]("users")

  def addUser(name: String): Future[Unit] = {
    val user = AnnotationToolUser(
      _id = BSONObjectID.generate(),
      token = java.lang.Long.toUnsignedString(RandomUtils.nextLong(), 36),
      admin = false,
      name = name
    )
    coll.insert(user).map(_ => ())
  }

  def setAdmin(id: BSONObjectID, to: Boolean): Future[Unit] = {
    val q = BSONDocument("_id" -> id)
    val upd = BSONDocument(
      "$set" -> BSONDocument(
        "admin" -> to
      )
    )
    coll.update(q, upd).map(_ => ())
  }

  def updateUser(u: AnnotationUser): Future[AnnotationToolUser] = {
    val id = BSONObjectID.parse(u.id)
    id match {
      case scala.util.Failure(ex) => Future.failed(ex)
      case scala.util.Success(uid) =>
        val q = BSONDocument("_id" -> uid)
        val upd = BSONDocument(
          "$set" -> BSONDocument(
            "token" -> u.token,
            "name" -> u.name
          )
        )
        coll.findAndUpdate(q, upd, fetchNewObject = true).map(_.result[AnnotationToolUser].get)
    }
  }

  def allUsers(): Future[AllUsers] = {
    coll
      .find(BSONDocument())
      .sort(BSONDocument("name" -> 1))
      .cursor[AnnotationToolUser]()
      .collect[Vector](-1, Cursor.FailOnError[Vector[AnnotationToolUser]]())
      .map { users =>
        AllUsers(
          users.map { u =>
            AnnotationUser(
              id = u._id.stringify,
              name = u.name,
              token = u.token,
              admin = u.admin
            )
          }
        )
      }
  }

  def checkToken(token: String): Future[Option[AnnotationToolUser]] = {
    val q = BSONDocument(
      "token" -> token
    )
    coll.find(q).one[AnnotationToolUser]
  }

  def findAnyAdmin(): Future[Option[AnnotationToolUser]] = {
    val q = BSONDocument(
      "admin" -> true
    )
    coll.find(q).one[AnnotationToolUser]
  }

  def save(o: AnnotationToolUser): Future[UpdateWriteResult] = {
    val q = BSONDocument(
      "_id" -> o._id
    )
    coll.update(q, o, upsert = true)
  }
}

case class BadStatusComment(
    @Key("_id") id: String,
    dateTime: DateTime,
    sentence: String,
    part: String,
    offset: Int,
    comment: String,
    annotatorId: String
)

object SentenceBSON {
  def enumFormat[T <: GeneratedEnum](
      implicit comp: GeneratedEnumCompanion[T]): BSONHandler[BSONInteger, T] =
    new BSONHandler[BSONInteger, T] {
      override def read(bson: BSONInteger): T = comp.fromValue(bson.value)
      override def write(t: T): BSONInteger = BSONInteger(t.value)
    }

  implicit def seqHandler[T](
      implicit h: BSONHandler[BSONValue, T]): BSONHandler[BSONArray, Seq[T]] =
    new BSONHandler[BSONArray, Seq[T]] {
      override def read(bson: BSONArray): Seq[T] = readTry(bson).get
      override def write(t: Seq[T]): BSONArray = {
        BSONArray(t.map(o => h.write(o)))
      }

      override def readTry(bson: BSONArray): Try[Seq[T]] = {
        bson.values
          .foldLeft(Try(Vector.newBuilder[T])) { (b, o) =>
            b match {
              case scala.util.Failure(_) => b
              case scala.util.Success(s) =>
                h.readTry(o).map { v =>
                  s += v
                  s
                }
            }
          }
          .map(_.result())
      }
    }

  implicit val objIdHandler: BSONHandler[BSONObjectID, ObjId] =
    new BSONHandler[BSONObjectID, ObjId] {
      override def read(bson: BSONObjectID): ObjId = ObjId(bson.stringify)
      override def write(t: ObjId): BSONObjectID = writeTry(t).get
      override def writeTry(t: ObjId): Try[BSONObjectID] = {
        BSONObjectID.parse(t.id)
      }
    }

  implicit val timestampHandler: BSONHandler[BSONDateTime, Timestamp] =
    new BSONHandler[BSONDateTime, Timestamp] {
      override def read(bson: BSONDateTime): Timestamp = {
        val secs = bson.value / 1000
        val nanos = (bson.value % 1000).toInt * 1000
        Timestamp(secs, nanos)
      }
      override def write(t: Timestamp): BSONDateTime = {
        val millis = t.nanos / 1000
        BSONDateTime(t.seconds * 1000 + millis)
      }
    }

  implicit val floatHandler: BSONHandler[BSONDouble, Float] =
    new BSONHandler[BSONDouble, Float] {
      override def read(bson: BSONDouble): Float = bson.value.toFloat
      override def write(t: Float): BSONDouble = BSONDouble(t.toDouble)
    }

  implicit val exampleTokenFormat: BSONDocumentHandler[ExampleToken] = Macros.handler[ExampleToken]
  implicit val annotationFormat: BSONDocumentHandler[Annotation] = Macros.handler[Annotation]
  implicit val tokenSpanFormat: BSONDocumentHandler[TokenSpan] = Macros.handler[TokenSpan]
  implicit val blockFormat: BSONDocumentHandler[SentenceBlock] = Macros.handler[SentenceBlock]
  implicit val statusFormat: BSONHandler[BSONInteger, SentenceStatus] = enumFormat[SentenceStatus]
  implicit val reviewFormat: BSONDocumentHandler[Review] = Macros.handler[Review]
  implicit val sentenceFormat: BSONDocumentHandler[Sentence] = Macros.handler[Sentence]

  case class SentenceIdReviews(@Key("_id") id: String, reviews: Seq[Review])
  implicit val sirFormat: BSONDocumentHandler[SentenceIdReviews] = Macros.handler[SentenceIdReviews]

  implicit val dateTimeHandler: BSONHandler[BSONDateTime, DateTime] =
    new BSONHandler[BSONDateTime, DateTime] {
      override def read(bson: BSONDateTime): DateTime = new DateTime(bson.value)
      override def write(t: DateTime): BSONDateTime = BSONDateTime(t.getMillis)
    }

  implicit val badStatusCommentFormat: BSONDocumentHandler[BadStatusComment] =
    Macros.handler[BadStatusComment]
}

class SentenceDbo @Inject()(db: AnnotationDb)(implicit ec: ExecutionContext) extends StrictLogging {

  private val coll = db.db.collection[BSONCollection]("sentences")

  import SentenceBSON._

  def recalcStatuses() = {
    coll.find(BSONDocument()).cursor[Sentence]().fold(NotUsed) {
      case (_, doc) =>
        val newStatus = SentenceUtils.computeStatus(doc)
        if (newStatus != doc.status) {
          val search = BSONDocument(
            "_id" -> doc.id
          )
          val upd = BSONDocument(
            "$set" -> BSONDocument(
              "status" -> newStatus.value
            )
          )
          coll.update(search, upd)
        }
        NotUsed
    }
  }

  def findAlreadyReported(surfaceComment: String): Future[Option[String]] = {
    val q = BSONDocument(
      "tags" -> "report",
      "originalComments" -> surfaceComment
    )

    val proj = BSONDocument("_id" -> 1)

    coll.find(q, proj).one[BSONDocument].map {
      case Some(doc) => doc.getAs[String]("_id")
      case _         => None
    }
  }

  def checkExistance(ids: Seq[String]): Future[Set[String]] = {
    if (ids.isEmpty) {
      return Future.successful(Set.empty)
    }

    val q = BSONDocument(
      "_id" -> BSONDocument(
        "$in" -> ids
      )
    )
    val vals = BSONDocument(
      "_id" -> 1
    )
    coll
      .find(q, vals)
      .cursor[BSONDocument]()
      .collect[Set](ids.size, Cursor.FailOnError[Set[BSONDocument]]())
      .map(_.map(_.getAs[String]("_id").get))
  }

  def saveSentences(data: Seq[Sentence]): Future[Int] = {
    coll.insert[Sentence](ordered = false).many(data).map(_.totalN)
  }

  private def convertCommonStatus(
      doc: BSONDocument,
      status: String,
      uid: BSONObjectID): BSONDocument = {
    import BSONElement.converted
    status match {
      case "new" =>
        doc.merge(
          "status" -> SentenceStatus.NotAnnotated.value
        )
      case "ok" =>
        doc.merge(
          "blocks.annotations.annotatorId" -> BSONDocument(
            "$ne" -> uid
          )
        )
      case _ => doc
    }
  }

  private def convertAdminStatus(doc: BSONDocument, status: String): BSONDocument = {
    status match {
      case "new" | "na" =>
        doc.merge(
          "status" -> SentenceStatus.NotAnnotated.value
        )
      case "ok" | "ta" =>
        doc.merge(
          "status" -> SentenceStatus.TotalAgreement.value
        )
      case "pa" =>
        doc.merge(
          "status" -> SentenceStatus.PartialAgreement.value
        )
      case "wr" =>
        doc.merge(
          "status" -> SentenceStatus.WorkRequired.value
        )
      case "rj" | "rej" =>
        doc.merge(
          "status" -> SentenceStatus.Rejected.value
        )
      case "bad" | "ng" =>
        doc.merge(
          "status" -> BSONDocument(
            "$gte" -> SentenceStatus.PartialAgreement.value
          )
        )
      case _ =>
        logger.warn(s"unsupported admin status: $status")
        doc
    }
  }

  def parseQuery(str: String, user: AnnotationToolUser): BSONDocument = {
    if (str.isEmpty) return BSONDocument.empty

    val parts = str.split("[ 　]+") //either half or full width whitespace
    parts.foldLeft(BSONDocument.empty) {
      case (doc, p) =>
        p.split("[:：]", 2) match {
          case Array(word) =>
            doc.merge(
              "blocks.spans.tokens.surface" -> BSONRegex(
                s"^$word",
                ""
              ))
          case Array(key, value) =>
            key match {
              case "t" | "tag" | "ｔ" => // tags
                doc.merge("tags" -> value)
              case "is" =>
                if (user.admin) convertAdminStatus(doc, value)
                else convertCommonStatus(doc, value, user._id)
              case "ann" | "ａｎｎ" =>
                doc.merge(
                  "blocks.annotations.value" -> value
                )
              case _ =>
                logger.warn(s"unsupported kv query part: [$key]:[$value]")
                doc
            }
          case _ =>
            logger.warn(s"unsupported query part: [$p]")
            doc
        }
    }
  }

  def stream(user: AnnotationToolUser, query: String): Source[Sentence, NotUsed] = {
    val sort = BSONDocument(
      "_id" -> 1
    )

    val q = parseQuery(query, user)

    import MongoStream._

    coll.find(q).sort(sort).stream[Sentence]()
  }

  def getSentences(user: AnnotationToolUser, req: GetSentences): Future[Sentences] = {
    var q = BSONDocument.empty

    if (req.exceptIds.nonEmpty) {
      q = q.merge(
        "_id" -> BSONDocument(
          "$nin" -> req.exceptIds
        )
      )
    }

    if (req.newForUser) {
      q = q.merge(
        "blocks.annotations.annotatorId" -> BSONDocument(
          "$ne" -> user._id
        )
      )
//      if (req.query == "") {
//        q = q.merge(
//          "status" -> BSONDocument(
//            "$ne" -> SentenceStatus.Rejected.value
//          )
//        )
//      }
    }

    if (req.reviewedBefore.isDefined) {
      val beforeDate = BSONDateTime(req.reviewedBefore.get.seconds * 1000)
      val annId = user._id

      val subQ = BSONDocument(
        "$or" -> BSONArray(
          BSONDocument(
            "reviews" -> BSONDocument(
              "$elemMatch" -> BSONDocument(
                "annotatorId" -> annId,
                "reviewedOn" -> BSONDocument(
                  "$lte" -> beforeDate
                )
              )
            )
          ),
          BSONDocument(
            "reviews" -> BSONDocument(
              "$not" -> BSONDocument(
                "$elemMatch" -> BSONDocument(
                  "annotatorId" -> annId
                )
              )
            )
          )
        )
      )

      q = q.merge(subQ)
    }

    q = q.merge(parseQuery(req.query, user))

    val max = if (req.limit == 0) 15 else req.limit

    val sort = BSONDocument(
      "_id" -> 1
    )

    logger.debug(s"search=${BSONDocument.pretty(q)}")

    val itemCount = coll.count(Some(q))

    val cursor = coll.find(q).skip(req.from).sort(sort).cursor[Sentence]()
    val items = cursor.collect(max, Cursor.FailOnError[Seq[Sentence]]())

    for {
      cnt <- itemCount
      data <- items
    } yield Sentences(data, cnt)
  }

  def publishReview(uid: BSONObjectID, sid: String): Future[Review] = {
    val q = BSONDocument(
      "_id" -> sid
    )

    val proj = BSONDocument(
      "reviews" -> 1
    )

    val search = coll.find(q, proj).requireOne[SentenceIdReviews]

    val now = BSONDateTime(System.currentTimeMillis())

    val review = Review(
      annotatorId = ObjId(uid.stringify),
      reviewedOn = Some(Timestamps.fromMillis(now.value))
    )

    search.flatMap { sir =>
      val filtered = sir.reviews.filter(_.annotatorId.id != uid.stringify)
      val reviews = filtered :+ review
      val upd = BSONDocument(
        "$set" -> BSONDocument(
          "reviews" -> BSON.write(reviews)
        )
      )
      coll.update(q, upd).map(_ => review)
    }
  }

  def annotate(req: Annotate, uid: BSONObjectID): Future[Annotation] = {
    val sentQuery = BSONDocument(
      "_id" -> req.sentenceId
    )

    val sentence = coll.find(sentQuery).requireOne[Sentence]

    sentence.flatMap { s =>
      val block = s.blocks.find(_.offset == req.offset).get
      val annId = ObjId(uid.stringify)
      val currentAnnotations = block.annotations
      val annIdx = currentAnnotations.indexWhere(_.annotatorId == annId)
      val newAnnotation = Annotation(
        annotatorId = ObjId(uid.stringify),
        value = req.annotation,
        comment = req.comment,
        duration = req.duration,
        timestamp = Some(Timestamps.now)
      )

      val newAnnotations = annIdx match {
        case _ if req.annotation == "" =>
          currentAnnotations.filterNot(_.annotatorId == annId)
        case -1 =>
          currentAnnotations ++ Seq(newAnnotation)
        case idx =>
          currentAnnotations.updated(idx, newAnnotation)
      }

      val modBlocks = s.blocks.map { b =>
        if (b.offset == req.offset) b.copy(annotations = newAnnotations)
        else b
      }

      val newSent = s.copy(blocks = modBlocks)

      val newAnnNumber = SentenceUtils.numAnnotations(newSent)
      val newStatus = SentenceUtils.computeStatus(newSent)

      val query = BSONDocument(
        "_id" -> req.sentenceId,
        "blocks.offset" -> req.offset
      )

      val update = BSONDocument(
        "$set" -> BSONDocument(
          "blocks.$.annotations" -> BSONArray(
            newAnnotations.map(a => annotationFormat.write(a))
          ),
          "status" -> newStatus,
          "numAnnotations" -> newAnnNumber
        )
      )

      coll
        .update(query, update)
        .flatMap(_ => publishReview(uid, req.sentenceId))
        .map(_ => newAnnotation)
    }
  }

  def mergeEdits(uid: BSONObjectID, req: MergeEdits): Future[Sentence] = {

    val q = BSONDocument(
      "_id" -> req.sentenceId
    )
    val saved = coll.find(q).requireOne[Sentence]

    saved.flatMap { s =>
      val merged = MergeSentence.merge(s, req.edits.get, ObjId(uid.stringify), req.duration)

      val updated = merged.copy(
        blocks = SentenceUtils.cleanTags(merged.blocks, AllowedFields.allowedFields),
        numAnnotations = SentenceUtils.numAnnotations(merged),
        status = SentenceUtils.computeStatus(merged)
      )

      coll
        .update(q, sentenceFormat.write(updated))
        .flatMap(_ => publishReview(uid, req.sentenceId))
        .map(_ => updated)
    }
  }

  def findById(id: String): Future[Option[Sentence]] = {
    val q = BSONDocument(
      "_id" -> id
    )
    coll.find(q).one[Sentence]
  }

  private val badColl = db.db.collection[BSONCollection]("badcomments")

  def saveBadLog(uid: BSONObjectID, req: ReportAllBad): Future[BadStatusComment] = {
    val sent = findById(req.sentenceId)
    sent.flatMap {
      case None => Future.failed(new Exception("no sentence exist"))
      case Some(s) =>
        val surface = s.blocks.flatMap(_.spans.head.tokens.map(_.surface)).mkString
        val focus = surface.substring(req.focusStart, req.focusEnd)
        val obj = BadStatusComment(
          id = BSONObjectID.generate().stringify,
          dateTime = DateTime.now(),
          sentence = surface,
          part = focus,
          offset = req.focusStart,
          comment = req.comment,
          annotatorId = uid.stringify
        )
        badColl.insert(obj).map(_ => obj)
    }
  }
}

object SentenceUtils {

  val badAnnotations = Set(
    "どうでもいい",
    "入力：意味不明",
    "入力：誤字脱字"
  )

  def cleanTags(blocks: Seq[SentenceBlock], allowedFields: Set[String]): Seq[SentenceBlock] = {
    blocks.map { b =>
      b.copy(spans = b.spans.map { s =>
        s.copy(tokens = s.tokens.map { t =>
          val newTags = t.tags.filter { case (k, _) => allowedFields.contains(k) }
          t.copy(tags = newTags)
        })
      })
    }
  }

  def numAnnotations(s: Sentence): Int = {
    s.blocks.flatMap(_.annotations).map(_.annotatorId).distinct.length
  }

  def computeStatus(s: Sentence): SentenceStatus = {
    val byBlock = s.blocks.map { b =>
      computeStatus(b.annotations)
    }

    if (byBlock.isEmpty) SentenceStatus.NotAnnotated
    else byBlock.maxBy(_.value)
  }

  def computeStatus(anns: Seq[Annotation]): SentenceStatus = {
    if (anns.isEmpty) {
      SentenceStatus.NotAnnotated
    } else {
      val values = anns.map(_.value).distinct
      values match {
        case vs if vs.exists(badAnnotations.contains)        => SentenceStatus.Rejected
        case Seq(XInt(_))                                    => SentenceStatus.TotalAgreement
        case vs if vs.forall(v => XInt.unapply(v).isDefined) => SentenceStatus.PartialAgreement
        case _                                               => SentenceStatus.WorkRequired
      }
    }
  }
}
