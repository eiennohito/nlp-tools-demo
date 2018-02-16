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
    midasi: String,
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
    features: Seq[JumanppFeature],
    scores: NodeScores
)

case class NodeScores(
    features: Float,
    languageModel: Float,
    morphAnalysis: Float
)

case class JumanppFeature(
    name: String,
    value: Option[String]
)

case class JumanppLattice(id: String, nodes: Seq[JumanppLatticeNode], comment: String)

case class AnalysisResult(
    id: String,
    date: String,
    text: String,
    lattice: JumanppLattice,
    version: String,
    dicVersion: String,
    fixes: Seq[Int]
)
