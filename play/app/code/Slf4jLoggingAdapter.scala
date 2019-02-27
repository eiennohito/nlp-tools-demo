package code
import akka.event.LoggingAdapter
import org.slf4j.Logger

final class Slf4jLoggingAdapter(log: Logger) extends LoggingAdapter {
  override def isErrorEnabled: Boolean = log.isErrorEnabled()
  override def isWarningEnabled: Boolean = log.isWarnEnabled
  override def isInfoEnabled: Boolean = log.isInfoEnabled()
  override def isDebugEnabled: Boolean = log.isDebugEnabled
  override protected def notifyError(message: String): Unit = log.error(message)
  override protected def notifyError(cause: Throwable, message: String): Unit = log.error(message, cause)
  override protected def notifyWarning(message: String): Unit = log.warn(message)
  override protected def notifyInfo(message: String): Unit = log.info(message)
  override protected def notifyDebug(message: String): Unit = log.debug(message)
}
