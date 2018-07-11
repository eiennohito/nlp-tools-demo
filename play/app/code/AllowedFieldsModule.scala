package code

import com.google.inject.Provides
import com.typesafe.scalalogging.StrictLogging
import javax.inject.Singleton
import net.codingwell.scalaguice.ScalaModule
import play.api.Configuration

class AllowedFields(val allowedFields: Set[String]) {
  def isAllowed(name: String): Boolean = allowedFields.contains(name)
}

class AllowedFieldsModule extends ScalaModule with StrictLogging {
  override def configure(): Unit = {}

  @Provides
  @Singleton
  def allowedFields(conf: Configuration): AllowedFields = {
    conf.getOptional[Seq[String]]("atool.fields") match {
      case Some(flds) =>
        logger.info("Allowed fields: {}", flds.mkString(", "))
        new AllowedFields(flds.toSet)
      case None =>
        logger.warn("No allowed fields are configured")
        new AllowedFields(Set.empty)
    }
  }
}
