package code.annotation

import java.util.Base64
import javax.inject.{Inject, Singleton}

import com.google.inject.Provides
import net.codingwell.scalaguice.ScalaModule
import org.apache.commons.lang3.RandomUtils
import play.api.Configuration
import play.api.mvc.Result
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.UpdateWriteResult
import reactivemongo.api.{Cursor, DefaultDB, MongoConnection}
import reactivemongo.bson.{
  BSONArray,
  BSONDocument,
  BSONDocumentHandler,
  BSONHandler,
  BSONInteger,
  BSONObjectID,
  BSONValue,
  BSONWriter,
  Macros
}
import ws.kotonoha.akane.akka.AnalyzerActor.Failure
import ws.kotonoha.akane.utils.XInt

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Success, Try}
import scalapb.{GeneratedEnum, GeneratedEnumCompanion}

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
  import prickle._
  import BsonPicklers._

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

  def allUsers(): Future[AllUsers] = {
    coll
      .find(BSONDocument())
      .sort(BSONDocument("name" -> 1))
      .cursor[AnnotationToolUser]()
      .collect[Vector](0, Cursor.FailOnError[Vector[AnnotationToolUser]]())
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

  implicit val exampleTokenFormat: BSONDocumentHandler[ExampleToken] = Macros.handler[ExampleToken]
  implicit val annotationFormat: BSONDocumentHandler[Annotation] = Macros.handler[Annotation]
  implicit val tokenSpanFormat: BSONDocumentHandler[TokenSpan] = Macros.handler[TokenSpan]
  implicit val blockFormat: BSONDocumentHandler[SentenceBlock] = Macros.handler[SentenceBlock]
  implicit val statusFormat: BSONHandler[BSONInteger, SentenceStatus] = enumFormat[SentenceStatus]
  implicit val sentenceFormat: BSONDocumentHandler[Sentence] = Macros.handler[Sentence]
}

class SentenceDbo @Inject()(db: AnnotationDb)(implicit ec: ExecutionContext) {

  private val coll = db.db.collection[BSONCollection]("sentences")

  import SentenceBSON._

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

  def getSentences(uid: BSONObjectID, req: GetSentences): Future[Seq[Sentence]] = {
    var q = BSONDocument(
      "_id" -> BSONDocument(
        "$nin" -> req.exceptIds
      )
    )

    if (req.newForUser) {
      val newq = BSONDocument(
        "blocks.annotations.annotatorId" -> BSONDocument(
          "$ne" -> uid
        )
      )
      q = q.merge(newq)
    }

    val max = if (req.limit == 0) 15 else req.limit

    val sort = BSONDocument(
      "_id" -> 1
    )

    coll.find(q).sort(sort).cursor[Sentence]().collect(max, Cursor.FailOnError[Seq[Sentence]]())
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
        comment = req.comment
      )

      val newAnnotations = annIdx match {
        case _ if req.annotation == "" =>
          currentAnnotations.filterNot(_.annotatorId == annId)
        case -1 =>
          currentAnnotations ++ Seq(newAnnotation)
        case idx =>
          currentAnnotations.updated(idx, newAnnotation)
      }

      val newAnnNumber = annIdx match {
        case -1 => Math.max(currentAnnotations.length + 1, s.numAnnotations)
        case _ if req.annotation == "" =>
          val otherNumber = s.blocks
            .filterNot(_.offset == req.offset)
            .map(_.annotations.length)
            .foldLeft(0)(_ max _)
          val myNumber = newAnnotations.length
          Math.max(otherNumber, myNumber)
        case _ => s.numAnnotations
      }

      val newStatus = (s.status, req.annotation) match {
        case (SentenceStatus.NotAnnotated | SentenceStatus.TotalAgreement, XInt(_)) =>
          if (currentAnnotations
                .filterNot(_.annotatorId == annId)
                .forall(v => v.value == req.annotation)) {
            SentenceStatus.TotalAgreement
          } else {
            SentenceStatus.PartialAgreement
          }
        case (SentenceStatus.PartialAgreement, XInt(_)) => SentenceStatus.PartialAgreement
        case _                                          => SentenceStatus.WorkRequired
      }

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

      coll.update(query, update).map(_ => newAnnotation)
    }
  }
}
