package code.annotation

import scala.scalajs.js.URIUtils

case class ReviewPageProps(
  showCounts: Boolean,
  search: GetSentences
)

object ReviewPageProps {
  val default = ReviewPageProps(
    showCounts = false,
    search = GetSentences(
      newForUser = true
    )
  )

  def formatQueryString(rpp: ReviewPageProps): Option[String] = {
    if (rpp == default) return None

    val bldr = new StringBuilder

    if (rpp.showCounts) {
      bldr.append("cnts=1&")
    }

    val srch = rpp.search

    if (!srch.newForUser) {
      bldr.append("nfu=0&")
    }

    if (srch.query != "") {
      bldr.append("q=")
      bldr.append(URIUtils.encodeURIComponent(srch.query))
      bldr.append("&")
    }

    val rb = srch.reviewedBefore

    if (rb.isDefined) {
      val obj = rb.get

      val date = new scala.scalajs.js.Date(obj.seconds * 1000)
      bldr.append("before=")
      bldr.append("%04d-%02d-%02d-%02d-%02d-%02d".format(
        date.getFullYear(),
        date.getMonth() + 1,
        date.getDate(),
        date.getHours(),
        date.getMinutes(),
        date.getSeconds()
      )).append("&")
    }

    bldr.append("data=none")

    Some(bldr.result())
  }

  def doParseString(str: String): ReviewPageProps = {
    var query = default.search
    var showCounts = default.showCounts

    val parts = str.split("&")
    for (p <- parts) {
      p.split("=") match {
        case Array("cnts", YInt(v)) => showCounts = v != 0
        case Array("nfu", YInt(v)) => query = query.copy(newForUser = v != 0)
        case Array("q", qs) => query = query.copy(query = URIUtils.decodeURIComponent(qs))
        case Array("before", dateString) =>
          val dateParts = dateString.split("-").flatMap(YInt.unapply)
          dateParts match {
            case Array(year, month, day, hour, minute, second) =>
              val date = new scala.scalajs.js.Date(year, month - 1, day, hour, minute, second)
              val ts = Timestamps.fromMillis(date.getTime().toLong)
              query = query.copy(reviewedBefore = Some(ts))
            case _ =>
          }
        case _ =>
      }
    }

    ReviewPageProps(
      showCounts, query
    )
  }

  def fromQueryString(param: Option[String]): ReviewPageProps = {
    param match {
      case None | Some("") => ReviewPageProps.default
      case Some(s) => doParseString(s)
    }
  }
}