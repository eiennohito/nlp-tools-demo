package code.annotation

import java.util.Base64

import code.transport.lattice.EditableSentence

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Dump")
object Dumper {
  @JSExport
  def dumpSentence(sent: Sentence): String = {
    val bytes = sent.toByteArray
    Base64.getEncoder.encodeToString(bytes)
  }

  @JSExport
  def dumpEdits(edits: EditableSentence): String = {
    val bytes = edits.toByteArray
    Base64.getEncoder.encodeToString(bytes)
  }
}
