package code

import com.google.inject.{Binder, Module, Provides, Singleton}
import org.joda.time.DateTime
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.{GetLastError, UpdateWriteResult, WriteResult}
import reactivemongo.api.{Cursor, DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.{
  BSONDateTime,
  BSONDocument,
  BSONDouble,
  BSONHandler,
  BSONObjectID,
  Macros
}
import ws.kotonoha.akane.analyzers.juman.{JumanFeature, JumanPos, JumanStringPos}
import ws.kotonoha.akane.analyzers.jumanpp.wire.{Lattice, LatticeNode, ScoreDetail}

import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * @author eiennohito
  * @since 2016/09/30
  */
class MongoModule extends Module {
  override def configure(binder: Binder): Unit = {}

  @Provides
  @Singleton
  def mongo(
      conf: Configuration,
      app: ApplicationLifecycle
  )(implicit ec: ExecutionContext): MongoConnection = {
    val driver = new MongoDriver(Some(conf.underlying), Some(getClass.getClassLoader))
    app.addStopHook { () =>
      Future { driver.close() }
    }
    val url = conf.get[String]("mongo.uri")
    driver.connection(MongoConnection.parseURI(url).get)
  }

  @Provides
  def worker(
      conn: MongoConnection,
      ec: ExecutionContext,
      cfg: Configuration
  ): MongoWorker = {
    import scala.concurrent.duration._
    val dbname = cfg.getOptional[String]("mongo.db").getOrElse("morph_demo")
    val db = Await.result(conn.database(dbname)(ec), 10.seconds)
    new MongoWorker(db)(ec)
  }
}

case class JppAnalysis(
    _id: BSONObjectID,
    timestamp: DateTime,
    input: String,
    analysis: Lattice,
    version: String,
    dicVersion: String,
    ip: String,
    userAgent: String,
    duration: Double,
    reported: Option[AnalysisReport]
)

case class AnalysisReport(nodes: Seq[Int])

object MongoObjects {
  implicit object dateTimeHandler extends BSONHandler[BSONDateTime, DateTime] {
    override def write(t: DateTime): BSONDateTime = BSONDateTime(t.getMillis)
    override def read(bson: BSONDateTime): DateTime = new DateTime(bson.value)
  }

  implicit object floatHandler extends BSONHandler[BSONDouble, Float] {
    override def write(t: Float): BSONDouble = BSONDouble(t)
    override def read(bson: BSONDouble): Float = bson.value.toFloat
  }

  implicit val reportHandler = Macros.handler[AnalysisReport]
  implicit val jumanOptHandler = Macros.handler[JumanFeature]
  implicit val jumanPosHandler = Macros.handler[JumanPos]
  implicit val jumanStrPosHandler = Macros.handler[JumanStringPos]
  implicit val scoreDetailHandler = Macros.handler[ScoreDetail]
  implicit val latticeNodeHandler = Macros.handler[LatticeNode]
  implicit val latticeHandler = Macros.handler[Lattice]
  implicit val ananlysisHandler = Macros.handler[JppAnalysis]
}

class MongoWorker(db: DefaultDB)(implicit ec: ExecutionContext) {
  private val coll = db.collection[BSONCollection]("code.analysis")

  import MongoObjects._

  def updateReport(id: String, ids: Seq[Int]): Future[UpdateWriteResult] = {
    val oid = BSONObjectID.parse(id).get

    val selector = BSONDocument("_id" -> oid)

    val report = AnalysisReport(ids)

    coll.update(
      selector,
      BSONDocument(
        "$set" -> BSONDocument(
          "reported" -> reportHandler.write(report)
        )
      ),
      writeConcern = GetLastError.Acknowledged,
      upsert = false,
      multi = false
    )
  }

  def save(analysis: JppAnalysis): Future[WriteResult] = {
    coll.insert(analysis)
  }

  def get(
      from: Int,
      limit: Int,
      fixed: Boolean,
      sorting: BSONDocument): Future[Seq[JppAnalysis]] = {
    val q =
      if (fixed)
        BSONDocument(
          "reported" -> BSONDocument(
            "$exists" -> true
          )
        )
      else BSONDocument.empty

    val qo = coll.find(q).sort(sorting)
    qo.options(qo.options.skip(from))
      .cursor[JppAnalysis]()
      .collect(limit, Cursor.FailOnError[Seq[JppAnalysis]]())
  }
}
