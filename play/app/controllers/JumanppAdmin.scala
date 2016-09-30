package controllers

import javax.inject.Inject

import code.{AnalysisResult, MongoWorker}
import org.joda.time.format.DateTimeFormat
import play.api.mvc.{Action, Controller}
import reactivemongo.bson.BSONDocument
import ws.kotonoha.akane.utils.XInt

import scala.concurrent.ExecutionContext

/**
  * @author eiennohito
  * @since 2016/09/30
  */
class JumanppAdmin @Inject() (
  mw: MongoWorker
)(implicit ec: ExecutionContext) extends Controller {

  def stats() = Action { Ok(views.html.jppadmin()) }

  def queries() = Action.async { req =>
    val from = req.getQueryString("from").flatMap(XInt.unapply).getOrElse(0)
    val fixed = req.getQueryString("fixed").contains("true")
    val sorting = req.getQueryString("sorting").collect({
      case "date" => BSONDocument("date" -> 1)
      case "date-" => BSONDocument("date" -> -1)
    }).getOrElse(BSONDocument("date" -> 1))

    mw.get(from, 100, fixed, sorting).map { items =>
      val dateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
      val data = items.map { a =>
        AnalysisResult(
          a._id.stringify,
          dateFormatter.print(a.timestamp),
          a.input,
          JumanppConversion.convertLatttice(a._id, a.analysis),
          a.version,
          a.dicVersion,
          a.reported.map(_.nodes)
        )
      }
      val string = upickle.default.write(data)
      Ok(string)
    }
  }
}
