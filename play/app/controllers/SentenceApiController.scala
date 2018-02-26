package controllers

import javax.inject.Inject

import code.annotation._
import play.api.mvc.{InjectedController, Result}

import scala.concurrent.{ExecutionContext, Future}

class SentenceApiController @Inject()(
    dbo: SentenceDbo
)(implicit ec: ExecutionContext)
    extends InjectedController {

  def doHandle(body: SentenceRequest, user: AnnotationToolUser): Future[Result] = {
    body.request match {
      case SentenceRequest.Request.Sentences(sents) =>
        dbo.getSentences(user._id, sents).map(sents => Ok(LiftPB(Sentences(sents))))
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
