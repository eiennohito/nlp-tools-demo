package tools

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZoneOffset}

import code.annotation.Sentence
import com.google.protobuf.timestamp.Timestamp
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{Cursor, MongoConnection, MongoDriver}
import reactivemongo.bson.{BSONArray, BSONDocument, BSONObjectID}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await

object CountAnnotations {

  import scala.concurrent.ExecutionContext.Implicits.global

  def localDate(ts: Timestamp): LocalDateTime = {
    LocalDateTime.ofEpochSecond(ts.seconds, ts.nanos, ZoneOffset.UTC)
  }

  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm")

  implicit val ldtOrdering: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) =>
    x.compareTo(y)

  def main(args: Array[String]): Unit = {
    import scala.concurrent.duration._
    val uri = MongoConnection.parseURI(args(0)).get
    val driver = new MongoDriver()
    val conn = driver.connection(uri)
    val db = Await.result(conn.database(uri.db.get), 10.seconds)

    val coll = db.collection[BSONCollection]("sentences")

    val annotators = Seq(
      "5b065f862300003368f7be93",
      "5b065f792300003368f7be92"
    )

    val q = BSONDocument(
      "blocks.annotations.annotatorId" -> BSONDocument(
        "$in" -> BSONArray(
          annotators.map(BSONObjectID.parse(_).get)
        )
      )
    )

    import code.annotation.SentenceBSON._

    val find = coll.find(q)
    val sents = Await.result(
      find.cursor[Sentence]().collect[Vector](-1, Cursor.FailOnError[Vector[Sentence]]()),
      1.minute)

    val perTime = new mutable.HashMap[String, Map[String, Int]]()
    val confusion = new mutable.HashMap[(String, String), Int]()
    val singles = new mutable.HashMap[(String, String), Int]()
    val durs = annotators.map(a => a -> new ArrayBuffer[Float]()).toMap

    for (s <- sents) {
      val data = s.blocks.flatMap { b =>
        b.annotations
          .filter(a => annotators.contains(a.annotatorId.id))
          .map(a => a.annotatorId.id -> localDate(a.timestamp.get))
      }
      data.groupBy(_._1).map { case (a, b) => a -> b.maxBy(_._2)._2 }.foreach {
        case (uid, date) =>
          val minute = date.getMinute
          val newMinute = (minute / 10) * 10
          val key = formatter.format(date.withMinute(newMinute))
          val map = perTime.getOrElseUpdate(key, Map.empty)
          perTime.put(key, map.updated(uid, map.getOrElse(uid, 0) + 1))
      }

      for (b <- s.blocks) {
        val anns =
          b.annotations.filter(a => annotators.contains(a.annotatorId.id)).sortBy(_.annotatorId.id)
        anns match {
          case Seq(x, y) =>
            val key = (x.value, y.value)
            confusion.update(key, confusion.getOrElse(key, 0) + 1)
          case Seq(x) =>
            val key = (x.annotatorId.id, x.value)
            singles.update(key, singles.getOrElse(key, 0) + 1)
          case _ =>
        }
      }

      val durData = s.blocks.flatMap { b =>
        b.annotations
          .filter(a => annotators.contains(a.annotatorId.id))
          .map(a => a.annotatorId.id -> a.duration)
      }
      durData.groupBy(_._1).map { case (a, b) => a -> b.maxBy(_._2)._2 }
      for ((k, v) <- durData) {
        durs(k) += v
      }
    }

    val dates = perTime.keys.toSeq.sorted

    println("======== Annotation Times: ==========")

    for (d <- dates) {
      val vals = perTime.getOrElse(d, Map.empty)
      val cnts = annotators.map { a =>
        vals.getOrElse(a, 0)
      }
      println(cnts.mkString(s"$d ", ", ", ""))
    }

    val cnts = perTime.foldLeft(Map.empty[String, Int]) {
      case (m1, m2) =>
        val x2 = m2._2
        x2.foldLeft(m1) { case (m, (k, v)) => m.updated(k, v + m.getOrElse(k, 0)) }
    }

    println(cnts)

    val keys = confusion.keys.flatMap { case (a, b) => Seq(a, b) }.toSeq.distinct.sorted

    println("======== Confusion Matrix: ==========")

    for (k1 <- keys) {
      print(k1)
      for (k2 <- keys) {
        print("\t")
        print(confusion.getOrElse((k1, k2), 0))
      }
      println()
    }

    println("=== DURATIONS: ====")
    for (d <- durs) {
      println(d._2.mkString("\t"))
    }

    driver.close()
  }
}
