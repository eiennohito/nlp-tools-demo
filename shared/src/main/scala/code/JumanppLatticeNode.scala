package code

/**
  * @author eiennohito
  * @since 2016/09/29
  */

case class JumanppLatticeNode(
  num: Int,
  startIdx: Int,
  surface: String,
  repr: String,
  reading: String,
  pos: String,
  subpos: String,
  conjtype: String,
  conjform: String,
  posId: Int,
  subposId: Int,
  conjtypeId: Int,
  conjformId: Int,
  previous: Seq[Int],
  rank: Seq[Int],
  features: Seq[JumanppFeature]
)

case class JumanppFeature(
  name: String,
  value: Option[String]
)

case class JumanppLattice(nodes: Seq[JumanppLatticeNode], comment: String)
