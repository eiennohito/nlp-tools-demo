package code.annotation

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.stream.{Attributes, Outlet, SourceShape}
import akka.stream.stage.{GraphStage, GraphStageLogic, OutHandler}
import com.typesafe.scalalogging.StrictLogging
import reactivemongo.api._
import reactivemongo.api.collections.GenericQueryBuilder

import scala.concurrent.{Future, Promise}

class CursorGraphStage[T, P <: SerializationPack, BP <: GenericQueryBuilder[P], R <: P#Reader[T]](
    bldr: BP,
    rpref: ReadPreference,
    maxDocs: Int
)(implicit rdr: R, cp: CursorProducer[T])
    extends GraphStage[SourceShape[T]]
    with StrictLogging {
  val out = Outlet[T]("out")
  val shape = SourceShape(out)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with OutHandler { logic =>

      setHandler(out, this)

      private var currentIter: Iterator[T] = Iterator.empty

      //is written from ReactiveMongo thread
      @volatile private var promise: Promise[Cursor.State[GraphStageLogic]] = null

      private def handleIterator() = {
        if (currentIter.hasNext) {
          push(out, currentIter.next())
        } else {
          if (promise != null) promise.trySuccess(Cursor.Cont(logic))
        }
      }

      override def onPull(): Unit = handleIterator()

      private val callback = getAsyncCallback[CursorGraphStage.Message] {
        case CursorGraphStage.Data(it) =>
          if (currentIter.hasNext) {
            failStage(new IllegalStateException("this guy should be empty"))
          } else {
            currentIter = it.asInstanceOf[Iterator[T]]
            if (isAvailable(out)) {
              handleIterator()
            }
          }
        case CursorGraphStage.Finish =>
          completeStage()
        case CursorGraphStage.Fail(t) =>
          failStage(t)
      }

      private def handleData(data: Iterator[T]): Future[Cursor.State[GraphStageLogic]] = {
        promise = Promise() //write from ReactiveMongo thread
        callback.invoke(CursorGraphStage.Data(data))
        promise.future
      }

      override def preStart(): Unit = {
        type Rdr2[TX] = bldr.pack.Reader[TX]
        val rdr2 = rdr.asInstanceOf[Rdr2[T]]
        val cursor = bldr.cursor[T](rpref)(rdr2, cp)
        cursor
          .foldBulksM[GraphStageLogic](logic, maxDocs)(
            (_, data) => handleData(data),
            Cursor.DoneOnError[GraphStageLogic]((_, err) =>
              callback.invoke(CursorGraphStage.Fail(err)))
          )(logic.materializer.executionContext)
          .onComplete {
            case scala.util.Success(l) =>
              callback.invoke(CursorGraphStage.Finish)
            case scala.util.Failure(ex) =>
              callback.invoke(CursorGraphStage.Fail(ex))
          }(logic.materializer.executionContext)
      }

      override def postStop(): Unit = {
        if (promise != null) promise.trySuccess(Cursor.Done(logic))
      }
    }
}

object CursorGraphStage {
  sealed trait Message
  case class Data(iter: Iterator[_]) extends Message
  case object Finish extends Message
  case class Fail(t: Throwable) extends Message
}

object MongoStream {
  implicit class QueryBuilderOps[QB <: GenericQueryBuilder[BSONSerializationPack.type]](
      val bldr: QB) {
    type Rdr[T] = bldr.pack.Reader[T]
    def stream[T](maxDocs: Int = -1, pref: ReadPreference = bldr.readPreference)(
        implicit rdr: Rdr[T],
        cp: CursorProducer[T]): Source[T, NotUsed] = {
      val stage = new CursorGraphStage[T, bldr.pack.type, QB, Rdr[T]](bldr, pref, maxDocs)(rdr, cp)
      Source.fromGraph(stage)
    }
  }
}
