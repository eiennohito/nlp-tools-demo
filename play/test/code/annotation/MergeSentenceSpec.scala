package code.annotation

import java.util.Base64

import code.transport.lattice.EditableSentence
import org.scalatest.{FreeSpec, Matchers}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

class MergeSentenceSpec extends FreeSpec with Matchers {

  def decode[T <: GeneratedMessage with Message[T]](data: String)(implicit comp: GeneratedMessageCompanion[T]): T = {
    comp.parseFrom(Base64.getDecoder.decode(data))
  }

  "MergeSentence" - {
    "merges correctly with different boundaries" in {
      val sentence = decode[Sentence]("ChR3MjAxMDA3LTAwODA2MDI4NDQtNhIwEi4SLAoq6Y6M5YCJ5pmC5Luj5pyr5pyf44Gu5qeY5byP44KS44KC44Gk5a+E5pyoErYBCA4SbBJqCgbpgKDjgooQARoSCghiYXNlZm9ybRIG6YCg44KLGh4KCGNvbmp0eXBlEhLlrZDpn7Pli5XoqZ7jg6nooYwaDQoDcG9zEgbli5XoqZ4aGwoIY29uamZvcm0SD+WfuuacrOmAo+eUqOW9ohJECAESQAoG6YCg44KKEAEaEAoDcG9zEgnmjqXlsL7ovp4aIgoGc3VicG9zEhjlkI3oqZ7mgKflkI3oqZ7mjqXlsL7ovp4SJggQEiISIAoe44CB5ryG566U5YOP44Gu5LuP5YOP44Gn44GZ44CCGhhzY29yZXM6IC0xLjcyMDYgLTEuODM0MTEaNOmAoOOCiglwb3M65o6l5bC+6L6eCXN1YnBvczrlkI3oqZ7mgKflkI3oqZ7mjqXlsL7ovp4aIVMtSUQ6dzIwMTAwNy0wMDgwNjAyODQ0LTYgQ09VTlQ6MjIEdGVzdDoKCLyZ+dQFEIjKHg==")
      val edits = decode[EditableSentence]("ChR3MjAxMDA3LTAwODA2MDI4NDQtNhImCiTpjozlgInmmYLku6PmnKvmnJ/jga7mp5jlvI/jgpLjgoLjgaQSlwEKCeWvhOacqOmAoBABGhQKB3N1cmZhY2USCeWvhOacqOmAoBoNCgNwb3MSBuWQjeipnhoWCgZzdWJwb3MSDOaZrumAmuWQjeipnhoVCghiYXNlZm9ybRIJ5a+E5pyo6YCgGhQKB3JlYWRpbmcSCeWvhOacqOmAoBoeCgdjYW5vbmljEhPlr4TmnKjpgKAv5a+E5pyo6YCgEiMKIeOCiuOAgea8hueulOWDj+OBruS7j+WDj+OBp+OBmeOAgg==")

      val sent = MergeSentence.merge(sentence, edits, ObjId(""), 0)
      sent.blocks should have length(3)
    }
  }
}
