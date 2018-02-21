package controllers

import javax.inject.Inject

import akka.stream.scaladsl.Sink
import akka.util.ByteString
import code.annotation._
import com.typesafe.scalalogging.StrictLogging
import play.api.data.Form
import play.api.http.Writeable
import play.api.libs.streams.Accumulator
import play.api.mvc._
import reactivemongo.bson.BSONObjectID
import ws.kotonoha.akane.io.ByteBufferInputStream

import scala.concurrent.{ExecutionContext, Future}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

case class TokenLogin(token: String)

object TokenLogin {

  import play.api.data.Forms._

  val form = Form(
    mapping(
      "token" -> text
    )(TokenLogin.apply)(TokenLogin.unapply)
  )
}

class Annotation @Inject()(
    auth: AnnotationAuth
)(implicit af: AssetsFinder, ec: ExecutionContext)
    extends InjectedController
    with StrictLogging {

  def default = Action { implicit req =>
    req.attrs.get(SessionUser.User) match {
      case None    => Ok(views.html.annotation.login())
      case Some(s) => Ok(views.html.annotation.loggedin(s.admin))
    }
  }

  def handleLogin = Action(parse.form(TokenLogin.form)).async { implicit req =>
    val login = req.body
    auth.checkToken(login.token).map {
      case Some(u) =>
        val userData = prickle.Pickle.intoString(u)
        logger.debug(s"logging user=${u.name}")
        Redirect(routes.Annotation.default()).withSession(
          SessionUser.SessionKey -> userData
        )
      case None =>
        logger.debug(s"login with token=${login.token} failed")
        Unauthorized("Invalid token")
    }
  }

  def logout() = Action { implicit req =>
    Redirect(routes.Annotation.default()).withNewSession
  }
}

class LiftPB[T <: GeneratedMessage](val message: T)

object LiftPB extends StrictLogging {
  import play.api.mvc.Results._

  private def toAkkaStreamByteString[T <: GeneratedMessage](msg: T): ByteString = {
    val bldr = ByteString.newBuilder
    val os = bldr.asOutputStream
    msg.writeTo(os)
    bldr.result()
  }

  implicit def liftWriteable[T <: GeneratedMessage]: Writeable[LiftPB[T]] = Writeable[LiftPB[T]](
    (v: LiftPB[T]) => toAkkaStreamByteString(v.message),
    Some("application/x-protobuf")
  )

  def apply[T <: GeneratedMessage](msg: T): LiftPB[T] = new LiftPB[T](msg)

  def protoBodyParser[A <: GeneratedMessage with Message[A]](
      implicit comp: GeneratedMessageCompanion[A],
      ec: ExecutionContext): BodyParser[A] = new BodyParser[A] {
    override def apply(hdr: RequestHeader): Accumulator[ByteString, Either[Result, A]] = {
      hdr.contentType match {
        case Some(
            "application/x-protobuf" | "application/x-google-protobuf" | "application/protobuf") =>
          val sink = Sink
            .fold(ByteString.empty) { (a, b: ByteString) =>
              a.concat(b)
            }
            .mapMaterializedValue { bytes =>
              bytes.map { bs =>
                try {
                  Right(comp.parseFrom(new ByteBufferInputStream(bs.toByteBuffer)))
                } catch {
                  case e: Exception =>
                    logger.error(s"Failed to parse protobuf for $comp", e)
                    Left(InternalServerError("Failed to parse protobuf"))
                }
              }
            }
          Accumulator.apply(sink)
        case _ =>
          Accumulator.done(
            Future.successful(Left(BadRequest("Was expecting protobuf context type"))))
      }
    }
  }
}

class AnnotationApi @Inject()(
    )(implicit ec: ExecutionContext)
    extends InjectedController {
  def root = Action {
    Ok("API ROOT")
  }
}

class AdminAction @Inject()(parser: BodyParsers.Default)(implicit ec: ExecutionContext)
    extends ActionBuilderImpl(parser) {
  override def invokeBlock[A](
      request: Request[A],
      block: Request[A] => Future[Result]): Future[Result] = {
    request.attrs.get(SessionUser.User) match {
      case Some(u) if u.admin =>
        block(request)
      case _ =>
        Future.successful(Results.Forbidden("Only admins can access here"))
    }
  }
}

class AnnotationUsersApiController @Inject()(
    auth: AnnotationAuth,
    admin: AdminAction
)(implicit ec: ExecutionContext)
    extends InjectedController {

  def handleList(): Future[AllUsers] = auth.allUsers()

  def handleAdd(cmd: AddUser): Future[Unit] = auth.addUser(cmd.name)

  def handleAdmin(cmd: ChangeAdminStatus): Future[Unit] = {
    Future.fromTry(BSONObjectID.parse(cmd.id)).flatMap(id => auth.setAdmin(id, cmd.status))
  }

  def handler = admin.async(LiftPB.protoBodyParser[AnnotationUserCommand]) { implicit req =>
    import AnnotationUserCommand.Command
    val objs = req.body.command match {
      case Command.Empty      => handleList()
      case Command.Add(add)   => handleAdd(add).flatMap(_ => handleList())
      case Command.Admin(cmd) => handleAdmin(cmd).flatMap(_ => handleList())
      case Command.List(_)    => handleList()
    }
    objs.map(u => Ok(LiftPB(u)))
  }

  def user = Action { implicit req =>
    req.attrs.get(SessionUser.User) match {
      case None => Unauthorized("No user")
      case Some(u) =>
        val pbUser = AnnotationUser(
          id = u._id.stringify,
          name = u.name,
          token = u.token,
          admin = u.admin
        )
        Ok(LiftPB(pbUser))
    }
  }

}
