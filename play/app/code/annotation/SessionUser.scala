package code.annotation

import javax.inject.Inject

import akka.stream.Materializer
import com.typesafe.scalalogging.StrictLogging
import play.api.libs.typedmap.TypedKey
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.{ExecutionContext, Future}

object SessionUser extends StrictLogging {
  val SessionKey = "auth-user"

  val User: TypedKey[AnnotationToolUser] = TypedKey("User")

  def getUser(rh: RequestHeader): Option[AnnotationToolUser] = {
    rh.session.get(SessionKey).flatMap { s =>
      prickle.Unpickle[AnnotationToolUser].fromString(s) match {
        case scala.util.Success(u) => Some(u)
        case scala.util.Failure(f) =>
          logger.warn("failed to unpickle user: ", f)
          None
      }
    }
  }
}

class UserFilter @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter {

  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {

    val req = SessionUser.getUser(rh) match {
      case Some(u) =>
        rh.withAttrs(rh.attrs.updated(SessionUser.User, u))
      case None => rh
    }

    f(req)
  }
}
