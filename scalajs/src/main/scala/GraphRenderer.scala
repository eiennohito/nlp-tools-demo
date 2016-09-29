import code.{JumanppLattice, JumanppLatticeNode}
import org.querki.jquery.{JQueryAjaxSettings, JQueryPromise, JQueryStatic}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport


@JSExport
object GraphRenderer {

  val jq = JQueryStatic

  val BOSnode = JumanppLatticeNode(
    0, 0,
    "BOS", "BOS", "",
    "", "", "", "",
    0, 0, 0, 0,
    Seq.empty, Seq.empty, Seq.empty
  )

  @JSExport
  def render(api: String, text: String): JQueryPromise = {
    val qurl = s"$api?text=$text"
    jq.ajax(qurl, JQueryAjaxSettings.contentType("text/plain")._result).`then`(handleResponse _)
  }

  def nid(n: JumanppLatticeNode) = s"N#${n.num}"

  var stored: JumanppLattice = null

  def handleResponse(resp: js.Any): js.Any = {
    import upickle.default._
    val data = read[JumanppLattice](resp.asInstanceOf[String])
    stored = data
    val graph = new DagreGraph().setGraph(GraphConfig())
    val byId = data.nodes.map(n => n.num -> n).toMap

    val totalSize = data.nodes.flatMap(_.rank).distinct.size

    graph.setNode(nid(BOSnode), NodeConfig(0, label = "BOS"))

    for (n <- data.nodes) {
      val nodeClasses = n.rank.map(r => s"rank-$r").mkString("simple ", " ", "")
      graph.setNode(nid(n), NodeConfig(n.num, label = renderNode(n), cls = nodeClasses))
      val prev = n.previous
      for (p <- prev) {
        val prevNode = byId.getOrElse(p, BOSnode)
        val commonRanks = if (prevNode.rank.isEmpty) n.rank
        else n.rank.intersect(prevNode.rank)
        if (commonRanks.nonEmpty) {
          val edgeClasses = commonRanks.map(r => s"rank-$r").mkString("connection ", " ", "")
          graph.setEdge(nid(prevNode), nid(n), EdgeConfig(renderEdgeLabel(commonRanks, totalSize), cls = edgeClasses))
        }
      }
    }

    graph
  }

  def compressSeq(iseq: IndexedSeq[Int]): String = {
    iseq.length match {
      case 0 => return ""
      case 1 => return iseq.head.toString
      case 2 => return iseq.mkString(",")
      case _ => //go further
    }

    val bldr = new StringBuilder

    var last = 0
    var i = 1
    val end = iseq.length - 1

    while (i <= end) {
      val x1 = iseq(last)
      val x2 = iseq(i)

      val valdiff = x2 - x1
      val idxDiff = i - last
      if (valdiff != idxDiff) {
        idxDiff match {
          case 1 =>
            bldr.append(x1)
            bldr.append(",\u200b") //word-breaks
            last = i
            i += 1
          case 2 =>
            bldr.append(x1)
            bldr.append(",\u200b")
            bldr.append(iseq(i - 1))
            bldr.append(",\u200b")
            last = i
            i += 1
          case _ =>
            bldr.append(x1)
            bldr.append("\u2060-\u2060") //and non-breaking space here
            bldr.append(iseq(i - 1))
            bldr.append(",\u200b")
            last = i
            i += 1
        }
      } else {
        i += 1
      }
    }

    val x1 = iseq(last)
    val x2 = iseq(end)
    val valDiff = x2 - x1
    val idxDiff = end - last
    if (valDiff == idxDiff && valDiff > 1) {
      bldr.append(x1).append("\u2060-\u2060").append(x2)
    } else bldr.append(x2)


    bldr.result()
  }

  private def renderEdgeLabel(common: Seq[Int], totalSize: Int) = {
    import scalatags.JsDom.all._

    div(
      `class` := "ranks",
     if (common.length == totalSize) "*" else compressSeq(common.toIndexedSeq)
    ).render
  }

  private def renderPos(n: JumanppLatticeNode) = {
    import scalatags.JsDom.all._

    def one(fn: JumanppLatticeNode => String, tpe: String, forDisplay: Map[String, String]) = {
      val value = fn(n)
      val content = forDisplay.getOrElse(value, value)
      div(
        `class` := s"pos ${if (value == "*") "absent" else ""}",
        title := value,
        content
      )
    }

    div(
      `class` := "pos-container",
      div(
        `class` := "pos-row",
        one(_.pos, "pos", PostagsForDisplay.pos),
        one(_.conjtype, "ctype", PostagsForDisplay.conj)
      ),
      div(
        `class` := "pos-row",
        one(_.subpos, "sub", PostagsForDisplay.sub),
        one(_.conjform, "cform", PostagsForDisplay.forms)
      )
    )
  }

  def renderNode(n: JumanppLatticeNode) = {
    import scalatags.JsDom.all._
    val tag = div(
      `class` := "node-label",
      div(
        `class` := "canonical",
        n.repr
      ),
      div(
        `class` := "surface",
        n.surface
      ),
      div(
        `class` := "pos-info",
        renderPos(n)
      )
    )

    tag.render
  }

  @JSExport
  def getReprForRank(rank: Int): String = {
    if (stored == null) ""
    else jumanRepr(stored, rank)
  }

  def jumanRepr(l: JumanppLattice, rank: Int): String = {
    val nodes = l.nodes.filter(_.rank.contains(rank))

    var lastStart = -1
    val bldr = new StringBuilder

    for (n <- nodes) {
      if (lastStart == n.startIdx) {
        bldr.append("@ ")
      }
      bldr.append(n.surface).append(" ")
      bldr.append(n.reading).append(" ")
      bldr.append(n.repr.substring(0, n.repr.indexOf('/'))).append(" ")

      bldr.append(n.pos).append(" ")
      bldr.append(n.posId).append(" ")
      bldr.append(n.subpos).append(" ")
      bldr.append(n.subposId).append(" ")
      bldr.append(n.conjtype).append(" ")
      bldr.append(n.conjtypeId).append(" ")
      bldr.append(n.conjform).append(" ")
      bldr.append(n.conjformId).append(" ")

      if (n.features.isEmpty && featureless.contains(n.pos)) {
        bldr.append("NIL")
      } else {
        bldr.append("\"")
        bldr.append("代表表記:").append(n.repr).append(" ")

        for (f <- n.features) {
          bldr.append(f.name)
          f.value match {
            case None =>
            case Some(v) =>
              bldr.append(":").append(v)
          }
          bldr.append(" ")
        }

        bldr(bldr.length - 1) = '"'
      }

      bldr.append("\n")
      lastStart = n.startIdx
    }
    bldr.append("EOS\n")
    bldr.result()
  }

  val featureless = Seq("指示詞", "助詞", "判定詞")
}


object PostagsForDisplay {
  val pos = Map(
    "*" -> "*",
    "特殊" -> "特",
    "動詞" -> "動",
    "形容詞" -> "形",
    "判定詞" -> "判",
    "助動詞" -> "助動",
    "名詞" -> "名",
    "指示詞" -> "指",
    "副詞" -> "副",
    "助詞" -> "助",
    "接続詞" -> "接",
    "連体詞" -> "連",
    "感動詞" -> "感",
    "接頭辞" -> "接頭",
    "接尾辞" -> "接尾",
    "未定義語" -> "未"
  )

  val sub = Map(
    "*" -> "*",
    "句点" -> "句",
    "読点" -> "読",
    "括弧始" -> "(",
    "括弧終" -> ")",
    "記号" -> "",
    "空白" -> "空",
    "普通名詞" -> "普",
    "サ変名詞" -> "サ",
    "固有名詞" -> "固",
    "地名" -> "地",
    "人名" -> "人",
    "組織名" -> "組",
    "数詞" -> "数",
    "形式名詞" -> "式",
    "副詞的名詞" -> "副",
    "時相名詞" -> "時",
    "名詞形態指示詞" -> "名指",
    "連体詞形態指示詞" -> "連指",
    "副詞形態指示詞" -> "副指",
    "格助詞" -> "格",
    "副助詞" -> "副",
    "接続助詞" -> "接",
    "終助詞" -> "終",
    "名詞接頭辞" -> "名",
    "動詞接頭辞" -> "動",
    "イ形容詞接頭辞" -> "イ形",
    "ナ形容詞接頭辞" -> "ナ形",
    "名詞性述語接尾辞" -> "名述接",
    "名詞性名詞接尾辞" -> "名接",
    "名詞性名詞助数辞" -> "名数",
    "名詞性特殊接尾辞" -> "名特",
    "形容詞性述語接尾辞" -> "形述接",
    "形容詞性名詞接尾辞" -> "形名接",
    "動詞性接尾辞" -> "動接",
    "その他" -> "他",
    "カタカナ" -> "カ",
    "アルファベット" -> "A")

  val conj = Map(
    "*" -> "*",
    "母音動詞" -> "母",
    "子音動詞カ行" -> "カ",
    "子音動詞カ行促音便形" -> "カb",
    "子音動詞ガ行" -> "ガ",
    "子音動詞サ行" -> "サ",
    "子音動詞タ行" -> "タ",
    "子音動詞ナ行" -> "ナ",
    "子音動詞バ行" -> "バ",
    "子音動詞マ行" -> "マ",
    "子音動詞ラ行" -> "ラ",
    "子音動詞ラ行イ形" -> "ライ",
    "子音動詞ワ行" -> "ワ",
    "子音動詞ワ行文語音便形" -> "ワ文",
    "カ変動詞" -> "カ変",
    "カ変動詞来" -> "カ来",
    "サ変動詞" -> "サ",
    "ザ変動詞" -> "ザ",
    "イ形容詞アウオ段" -> "イ形1",
    "イ形容詞イ段" -> "イ形2",
    "イ形容詞イ段特殊" -> "イ形2a",
    "ナ形容詞" -> "ナ形",
    "ナノ形容詞" -> "ナノ形",
    "ナ形容詞特殊" -> "ナ形s",
    "タル形容詞" -> "タル形",
    "判定詞" -> "判",
    "無活用型" -> "無",
    "助動詞ぬ型" -> "助ぬ",
    "助動詞だろう型" -> "助だろ",
    "助動詞そうだ型" -> "助そう",
    "助動詞く型" -> "助く",
    "動詞性接尾辞ます型" -> "ます",
    "動詞性接尾辞うる型" -> "うる")

  val forms = Map(
    "*" -> "*",
    "語幹" -> "幹",
    "基本形" -> "基",
    "未然形" -> "未",
    "意志形" -> "志",
    "省略意志形" -> "志2",
    "命令形" -> "命",
    "基本条件形" -> "条",
    "基本連用形" -> "連",
    "タ接連用形" -> "タ連",
    "タ形" -> "タ",
    "タ系推量形" -> "タ推",
    "タ系省略推量形" -> "タ推2",
    "タ系条件形" -> "タ条",
    "タ系連用テ形" -> "テ",
    "タ系連用タリ形" -> "タリ",
    "タ系連用チャ形" -> "チャ",
    "音便条件形" -> "条r",
    "文語命令形" -> "文命",
    "文語基本形" -> "文基",
    "文語未然形" -> "文未",
    "基本推量形" -> "推",
    "基本省略推量形" -> "推r",
    "タ系連用チャ形２" -> "チャ2",
    "音便条件形２" -> "条3",
    "文語連用形" -> "文連",
    "文語連体形" -> "文体",
    "エ基本形" -> "エ基",
    "ダ列基本連体形" -> "基",
    "ダ列基本推量形" -> "推",
    "ダ列基本省略推量形" -> "推r",
    "ダ列基本条件形" -> "条",
    "ダ列基本連用形" -> "連",
    "ダ列タ形" -> "タ",
    "ダ列タ系推量形" -> "タ推",
    "ダ列タ系省略推量形" -> "タ推r",
    "ダ列タ系条件形" -> "ダ列タ系条件形",
    "ダ列タ系連用テ形" -> "テ",
    "ダ列タ系連用タリ形" -> "ダ列タ系連用タリ形",
    "ダ列タ系連用ジャ形" -> "ダ列タ系連用ジャ形",
    "ダ列文語連体形" -> "ダ列文語連体形",
    "ダ列文語条件形" -> "ダ列文語条件形",
    "デアル列基本形" -> "デアル列基本形",
    "デアル列命令形" -> "デアル列命令形",
    "デアル列基本推量形" -> "デアル列基本推量形",
    "デアル列基本省略推量形" -> "デアル列基本省略推量形",
    "デアル列基本条件形" -> "デアル列基本条件形",
    "デアル列基本連用形" -> "デアル列基本連用形",
    "デアル列タ形" -> "デアル列タ形",
    "デアル列タ系推量形" -> "デアル列タ系推量形",
    "デアル列タ系省略推量形" -> "デアル列タ系省略推量形",
    "デアル列タ系条件形" -> "デアル列タ系条件形",
    "デアル列タ系連用テ形" -> "デアル列タ系連用テ形",
    "デアル列タ系連用タリ形" -> "デアル列タ系連用タリ形",
    "デス列基本形" -> "デス列基本形",
    "デス列音便基本形" -> "デス列音便基本形",
    "デス列基本推量形" -> "デス列基本推量形",
    "デス列音便基本推量形" -> "デス列音便基本推量形",
    "デス列基本省略推量形" -> "デス列基本省略推量形",
    "デス列音便基本省略推量形" -> "デス列音便基本省略推量形",
    "デス列タ形" -> "デス列タ形",
    "デス列タ系推量形" -> "デス列タ系推量形",
    "デス列タ系省略推量形" -> "デス列タ系省略推量形",
    "デス列タ系条件形" -> "デス列タ系条件形",
    "デス列タ系連用テ形" -> "デス列タ系連用テ形",
    "デス列タ系連用タリ形" -> "デス列タ系連用タリ形",
    "ヤ列基本形" -> "ヤ列基本形",
    "ヤ列基本推量形" -> "ヤ列基本推量形",
    "ヤ列基本省略推量形" -> "ヤ列基本省略推量形",
    "ヤ列タ形" -> "ヤ列タ形",
    "ヤ列タ系推量形" -> "ヤ列タ系推量形",
    "ヤ列タ系省略推量形" -> "ヤ列タ系省略推量形",
    "ヤ列タ系条件形" -> "ヤ列タ系条件形",
    "ヤ列タ系連用タリ形" -> "ヤ列タ系連用タリ形",
    "ダ列特殊連体形" -> "ダ列特殊連体形",
    "ダ列特殊連用形" -> "ダ列特殊連用形",
    "音便基本形" -> "音便基本形",
    "音便推量形" -> "音便推量形",
    "音便省略推量形" -> "音便省略推量形",
    "文語条件形" -> "文語条件形",
    "文語音便条件形" -> "文語音便条件形")
}
