package code.annotation

import scalapb.TypeMapper

case class ObjId(id: String) extends AnyVal {
  override def toString: String = id
}

object ObjId {
  implicit val typeMapper: TypeMapper[String, ObjId] = TypeMapper(ObjId.apply)(_.id)
}
