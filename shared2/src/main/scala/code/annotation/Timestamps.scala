package code.annotation

import com.google.protobuf.timestamp.Timestamp

object Timestamps {
  def now: Timestamp = {
    val millis = System.currentTimeMillis()
    fromMillis(millis)
  }

  def fromMillis(millis: Long): Timestamp = {
    val secs = millis / 1000
    val nanos = (millis % 1000).toInt * 1000
    Timestamp(secs, nanos)
  }
}
