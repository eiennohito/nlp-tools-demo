package controllers

import javax.inject.Inject

import code.annotation._
import play.api.mvc.{InjectedController, Result}
import reactivemongo.bson.BSONObjectID

import scala.concurrent.{ExecutionContext, Future}

class SentenceApiController @Inject()(
    dbo: SentenceDbo
)(implicit ec: ExecutionContext)
    extends InjectedController {

  def doHandle(body: SentenceRequest, user: AnnotationToolUser): Future[Result] = {
    body.request match {
      case SentenceRequest.Request.Sentences(sents) =>
        dbo.getSentences(user, sents).map(sents => Ok(LiftPB(sents)))
      case SentenceRequest.Request.Annotate(obj) =>
        val uid = if (user.admin) {
          BSONObjectID.parse(obj.annotatorId.id).getOrElse(user._id)
        } else user._id
        dbo.annotate(obj, uid).map(x => Ok(LiftPB(x)))
      case SentenceRequest.Request.Merge(req) =>
        val uid = if (user.admin) {
          BSONObjectID.parse(req.annotatorId.id).getOrElse(user._id)
        } else user._id
        if (req.edits.isEmpty) {
          Future.successful(BadRequest("Edits are empty"))
        } else {
          dbo.mergeEdits(uid, req).map(x => Ok(LiftPB(x)))
        }
      case _ => Future.successful(NotImplemented)
    }
  }

  def handler() = Action.async(LiftPB.protoBodyParser[SentenceRequest]) { implicit req =>
    req.attrs.get(SessionUser.User) match {
      case Some(u) => doHandle(req.body, u)
      case None    => Future.successful(Unauthorized("You are not allowed here"))
    }
  }
}
