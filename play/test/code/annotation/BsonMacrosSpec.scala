package code.annotation
import org.eiennohito.bson.Macros2
import org.scalatest.{FreeSpec, Matchers}
import reactivemongo.bson.BSONDocument

object BsonMacrosSpec {
  case class A(i: Int = 2, j: String, k: Double = 0)

  val rd = Macros2.readerOpts[A, Macros2.Options.Verbose]
}

class BsonMacrosSpec extends FreeSpec with Matchers {
  "something generates" in {
    val obj = BsonMacrosSpec.rd.read(BSONDocument("j" -> "5"))
    obj.i shouldBe 2
    obj.j shouldBe "5"
    obj.k shouldBe 0
  }
}
