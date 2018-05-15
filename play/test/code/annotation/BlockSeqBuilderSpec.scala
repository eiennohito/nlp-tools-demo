package code.annotation

import code.transport.lattice.{CandidateNode, EditableSentencePart, NodeTag}
import controllers.BlockSeqBuilder
import org.scalatest.{FreeSpec, Matchers}


class BlockSeqBuilderSpec extends FreeSpec with Matchers {

  val nouns = Seq(NodeTag("pos", "n"))
  val verbs = Seq(NodeTag("pos", "v"))

  "BlockSeqBuilder" - {
    "merges stuff correctly" in {
      val bsb = new BlockSeqBuilder
      val top1 = Seq(
        CandidateNode(
          surface = "what",
          tags = nouns
        ),
        CandidateNode(
          surface = "is",
          tags = verbs
        ),
        CandidateNode(
          surface = "it",
          tags = nouns
        )
      )
      val parts = Seq(
        EditableSentencePart(
          "what"
        ),
        EditableSentencePart(
          "is",
          node = true,
          tags = nouns
        ),
        EditableSentencePart(
          "it"
        )
      )
      bsb.make(top1, parts)
      val res = bsb.result()
      res should have length(3)
      val mid = res(1)
      mid.spans should have length(2)
      mid.spans(0).tokens should have length 1
      mid.spans(0).tokens(0).tags("pos") shouldBe "n"
      mid.spans(1).tokens should have length 1
      mid.spans(1).tokens(0).tags("pos") shouldBe "v"
    }

    "merges overlapping stuff correctly" in {
      val bsb = new BlockSeqBuilder
      val top1 = Seq(
        CandidateNode(
          surface = "wha",
          tags = nouns
        ),
        CandidateNode(
          surface = "ti",
          tags = verbs
        ),
        CandidateNode(
          surface = "st",
          tags = verbs
        ),
        CandidateNode(
          surface = "hat",
          tags = nouns
        )
      )
      val parts = Seq(
        EditableSentencePart(
          "what"
        ),
        EditableSentencePart(
          "is",
          node = true,
          tags = nouns
        ),
        EditableSentencePart(
          "that"
        )
      )

      bsb.make(top1, parts)
      val res = bsb.result()
      res should have length(3)
      val mid = res(1)
      mid.spans should have length(2)
      mid.spans(0).tokens should have length 3
      mid.spans(0).tokens(0).surface shouldBe "t"
      mid.spans(0).tokens(1).surface shouldBe "is"
      mid.spans(0).tokens(2).surface shouldBe "t"
      mid.spans(0).tokens(1).tags("pos") shouldBe "n"
      mid.spans(1).tokens should have length 2
      mid.spans(1).tokens(0).tags("pos") shouldBe "v"
    }

  }
}
