package code.annotation

import java.util.Base64
import javax.inject.Singleton

import com.google.inject.Provides
import net.codingwell.scalaguice.ScalaModule
import org.apache.commons.lang3.RandomUtils
import play.api.Configuration
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.UpdateWriteResult
import reactivemongo.api.{Cursor, DefaultDB, MongoConnection}
import reactivemongo.bson.{BSONDocument, BSONDocumentHandler, BSONObjectID, Macros}

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
