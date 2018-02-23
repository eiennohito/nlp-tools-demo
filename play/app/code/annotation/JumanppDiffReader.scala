package code.annotation

import java.io.BufferedReader
import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

case class ReadComments(raw: Seq[String], id: String, parts: Seq[ExampleToken])

object JumanppDiffReader {

  def readExample(rdr: BufferedReader): Try[Option[Sentence]] = {
    val comments = new ArrayBuffer[String]()
    val lines = new ArrayBuffer[String]()

    var line = ""
    while ({
      line = rdr.readLine()
      line != null
    }) {
      if (line.isEmpty) {
        return readExample(comments, lines).map(x => Some(x))
      }
      if (line.length > 2 && line.charAt(0) == '#' && line.charAt(1) == ' ') {
        comments += line.drop(2)
      } else {
        lines += line
      }
    }

    if (comments.isEmpty && lines.isEmpty) {
      return Success(None)
    }

    readExample(comments, lines).map(x => Some(x))
  }

  private val eosPattern = Pattern.compile("\n|\n\r|\r\n")

  def readExample(data: CharSequence): Try[Sentence] = {
    val comments = new ArrayBuffer[String]()
    val lines = new ArrayBuffer[String]()

    val matcher = eosPattern.matcher(data)
    var start = 0
    while (matcher.find()) {
      val end = matcher.start()
      val length = end - start
      if (length > 2 && data.charAt(start) == '#' && data.charAt(start + 1) == ' ') {
        comments += data.subSequence(start + 2, end).toString
      } else {
        lines += data.subSequence(start, end).toString
      }
      start = matcher.end()
    }
    if (start != 0) {
      lines += data.subSequence(start, data.length()).toString
    }

    readExample(comments, lines)
  }

  def readExample(commentData: Seq[String], bodyData: Seq[String]): Try[Sentence] = {
    for {
      comments <- readComments(commentData)
      body <- readBody(bodyData, comments)
    } yield
      Sentence(
        id = comments.id,
        blocks = body,
        originalComments = comments.raw
      )
  }

  private def parseToken(str: String): ExampleToken = {
    val data = str.split("\t")
    val surface = data(0)
    val tags = data.view
      .drop(1)
      .map(s =>
        s.split(":") match {
          case Array(k, v) => k -> v
          case _           => throw new DiffParseException(s"Invalid tag: $s in token $str")
      })
      .toMap

    ExampleToken(
      surface = surface,
      unit = true,
      tags = tags
    )
  }

  private val idPrefix = "S-ID:"

  private def readComments(data: Seq[String]): Try[ReadComments] = {
    val iter = data.iterator
    val tokens = Seq.newBuilder[ExampleToken]
    var id = ""
    while (iter.hasNext) {
      val s = iter.next()
      if (s.startsWith(idPrefix)) {
        val idx = s.indexOf(' ', idPrefix.length)
        if (idx == -1) {
          id = s.substring(idPrefix.length)
        } else {
          id = s.substring(idPrefix.length, idx)
        }
      } else if (s.indexOf('\t') >= 0) try {
        tokens += parseToken(s)
      } catch {
        case e: Exception => return Failure(e)
      }
    }
    Success(
      ReadComments(
        data,
        id,
        tokens.result()
      )
    )
  }

  private def makeCommon(offset: Int, data: String): SentenceBlock = {
    SentenceBlock(
      offset = offset,
      Seq(
        TokenSpan(
          tokens = Seq(
            ExampleToken(
              surface = data
            )
          )
        )
      )
    )
  }

  private def readBody(data: Seq[String], comments: ReadComments): Try[Seq[SentenceBlock]] =
    Try(readBodyUnsafe(data, comments))

  def makeDiff(
      offset: Int,
      currentSpan: Seq[ExampleToken],
      tokens: Iterator[ExampleToken]): SentenceBlock = {
    val length = currentSpan.view.map(_.surface.length).sum
    val otherSeq = Seq.newBuilder[ExampleToken]
    var otherLength = 0

    while (tokens.hasNext && otherLength < length) {
      val next = tokens.next()
      otherLength += next.surface.length
      otherSeq += next
    }

    if (otherLength != length) {
      throw new DiffParseException(
        s"Diff had different length: $length vs $otherLength [$currentSpan, $otherSeq]")
    }

    SentenceBlock(
      offset = offset,
      Seq(
        TokenSpan(
          tokens = currentSpan
        ),
        TokenSpan(
          index = 1,
          tokens = otherSeq.result()
        )
      )
    )
  }

  private def readBodyUnsafe(data: Seq[String], comments: ReadComments): Seq[SentenceBlock] = {
    val dataIter = data.iterator

    val commentIter = comments.parts.iterator
    var currentSpan = new ArrayBuffer[ExampleToken]()
    var offset = 0
    var diffOffset = -1
    val blocks = Seq.newBuilder[SentenceBlock]

    while (dataIter.hasNext) {
      val line = dataIter.next()

      if (line.startsWith("\t")) {
        if (diffOffset == -1) {
          diffOffset = offset
        }
        val token = parseToken(line.substring(1))
        currentSpan += token
        offset += token.surface.codePointCount(0, token.surface.length)
      } else {
        if (currentSpan.nonEmpty) {
          blocks += makeDiff(diffOffset, currentSpan, commentIter)
          currentSpan = new ArrayBuffer[ExampleToken]()
          diffOffset = -1
        }

        blocks += makeCommon(offset, line)
        offset += line.codePointCount(0, line.length)
      }
    }

    if (currentSpan.nonEmpty) {
      blocks += makeDiff(diffOffset, currentSpan, commentIter)
    }

    if (commentIter.hasNext) {
      throw new DiffParseException(
        s"comment tokens were not all used, remaining: ${commentIter.toList}")
    }

    blocks.result()
  }
}

class DiffParseException(msg: String) extends RuntimeException(msg)
