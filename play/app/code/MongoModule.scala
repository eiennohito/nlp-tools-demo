package code

import com.google.inject.{Binder, Module, Provides, Singleton}
import org.joda.time.DateTime
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.{BSONDateTime, BSONDouble, BSONHandler, BSONObjectID, Macros}
import ws.kotonoha.akane.analyzers.juman.{JumanOption, JumanPos}
import ws.kotonoha.akane.analyzers.jumanpp.wire.{Lattice, LatticeNode}

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
    val driver = new MongoDriver(Some(conf.underlying))

    app.addStopHook { () => Future{ driver.close() } }

    val url = conf.getString("mongo.uri")
    driver.connection(MongoConnection.parseURI(url.get).get)
  }

  @Provides
  def worker(
    conn: MongoConnection,
    ec: ExecutionContext
  ): MongoWorker = {
    import scala.concurrent.duration._
    val db = Await.result(conn.database("morph_demo")(ec), 2.seconds)
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
  implicit val jumanOptHandler = Macros.handler[JumanOption]
  implicit val jumanPosHandler = Macros.handler[JumanPos]
  implicit val latticeNodeHandler = Macros.handler[LatticeNode]
  implicit val latticeHandler = Macros.handler[Lattice]
  implicit val ananlysisHandler = Macros.handler[JppAnalysis]
}

class MongoWorker(db: DefaultDB)(implicit ec: ExecutionContext) {

  private val coll = db.collection[BSONCollection]("analysis")

  import MongoObjects._

  def save(analysis: JppAnalysis): Future[WriteResult] = {
    coll.insert(analysis)
  }

}
