package code.annotation

import org.scalatest.{FreeSpec, Matchers}

class JumanppDiffReaderSpec extends FreeSpec with Matchers {
  "JumanDiffReader" - {
    "Can read an example" in {
      val example =
        """# scores: -4.58982 -7.27708
          |# とい	pos:名詞	subpos:普通名詞
          |# う	pos:名詞	subpos:普通名詞
          |# S-ID:w201007-0080601370-49 COUNT:2
          |中には身重の女性の局部に銃剣をさして
          |	と	pos:助詞	subpos:格助詞
          |	いう	conjtype:子音動詞ワ行	pos:動詞	conjform:基本形
          |ような写真が。＞""".stripMargin
      val parsed = JumanppDiffReader.readExample(example)
      val ex = parsed.get
      ex.blocks should have length 3
      val firstBlock = ex.blocks.head
      firstBlock.spans.head.tokens.head.surface shouldBe "中には身重の女性の局部に銃剣をさして"
      val secondBlock = ex.blocks(1)
      secondBlock.offset shouldBe "中には身重の女性の局部に銃剣をさして".length
      secondBlock.spans should have length 2
      secondBlock.spans.head.tokens should have length 2
      val firstSurf = secondBlock.spans(0).tokens.map(_.surface).mkString
      val secondSurf = secondBlock.spans(1).tokens.map(_.surface).mkString
      firstSurf shouldBe secondSurf
      secondBlock.spans(1).tokens should have length 2
      val thirdBlock = ex.blocks(2)
      thirdBlock.offset shouldBe (secondBlock.offset + 3)
    }
  }
}
