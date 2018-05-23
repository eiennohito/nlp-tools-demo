package code.annotation

import javax.inject.Inject
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.Subtype.GenericBinarySubtype
import reactivemongo.bson.{BSONBinary, BSONDateTime, BSONDocument, BSONObjectID}

import scala.concurrent.ExecutionContext

class LogDbo @Inject()(
    db: AnnotationDb
)(implicit ec: ExecutionContext) {

  private val coll = db.db.collection[BSONCollection]("sentenceActions")

  def log(req: SentenceRequest, user: AnnotationToolUser): Unit = {
    val doc = BSONDocument(
      "_id" -> BSONObjectID.generate(),
      "user" -> user._id,
      "kind" -> req.request.number,
      "date" -> BSONDateTime(System.currentTimeMillis()),
      "blob" -> BSONBinary(req.toByteArray, GenericBinarySubtype)
    )
    coll.insert(doc)
  }
}
