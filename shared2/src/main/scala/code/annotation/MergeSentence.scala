package code.annotation

import code.transport.lattice.{EditableSentence, EditableSentencePart}

import scala.collection.mutable.ArrayBuffer

object MergeSentence {

  def clampVals(vals: Seq[Annotation]): Seq[Annotation] = {
    if (vals.map(_.value).distinct.size == 1) {
      Seq(vals.head)
    } else Nil
  }

  def mergeAnnotations(a1: Seq[Annotation], a2: Seq[Annotation]): Seq[Annotation] = {
    if (a1.isEmpty) return a2
    if (a2.isEmpty) return a1

    val groups = (a1 ++ a2).groupBy(a => (a.annotatorId, a.value))
    groups.flatMap {
      case (_, vals) =>
        if (vals.size == 1) {
          vals
        } else {
          clampVals(vals)
        }
    }.toVector
  }

  def noTags(block: SentenceBlock): Boolean = {
    block.spans.length == 1 && block.spans.forall(_.tokens.forall(t => !t.unit && t.tags.isEmpty))
  }

  def noTags(part: EditableSentencePart): Boolean = {
    !part.node && part.tags.isEmpty
  }

  class TokenSpanBuilder(val idx: Int, private var annotations: Seq[Annotation]) {
    private var tokens = new ArrayBuffer[ExampleToken]()

    def appendSpan(span: TokenSpan): Unit = {
      tokens ++= span.tokens
      annotations = mergeAnnotations(annotations, span.annotations)
    }

    def contentClone(newIdx: Int): TokenSpanBuilder = {
      val bldr = new TokenSpanBuilder(newIdx, Nil)
      bldr.tokens ++= tokens
      bldr
    }

    def isFullMatch(parts: Seq[PlannedPart]): Boolean = {
      val i1 = parts.iterator
      val i2 = tokens.iterator

      while (i1.hasNext && i2.hasNext) {
        val v1 = i1.next()
        val v2 = i2.next()

        if (v1.part.surface != v2.surface) {
          return false
        }

        if (!v1.noTags ^ v2.unit) {
          return false
        }

        val tags = v2.tags
        val partTags = v1.part.tags.map(t => t.key -> t.value).toMap
        if (tags.exists { case (k, v) => partTags.get(k).exists(_ != v) }) {
          return false
        }
      }

      !(i1.hasNext && i2.hasNext)
    }

    def mismatch(parts: Seq[PlannedPart]): Int = {
      val tokIter = tokens.iterator
      val partIter = parts.iterator

      var tokStart = 0
      var partStart = 0
      var score = 0

      while (tokIter.hasNext && partIter.hasNext) {
        val nextTok = tokIter.next()
        val nextPart = partIter.next()
        val blockEnd = tokStart + nextTok.surface.length
        val partEnd = partStart + nextPart.length

        if (blockEnd == partEnd) {
          val tags = nextTok.tags
          val partTags = nextPart.part.tags.map(t => t.key -> t.value).toMap

          score += tags.count { case (k, v) => partTags.get(k).exists(_ != v) }

          tokStart = blockEnd
          partStart = partEnd
        } else {
          return Int.MaxValue
        }
      }

      score
    }

    def clearTokens(): Unit = {
      tokens.clear()
    }

    def appendTokens(parts: Seq[PlannedPart], from: Int, to: Int): Unit = {
      for (px <- parts) {
        val p = px.part

        val token = if (px.start < from || px.end > to) {
          assert(px.noTags)
          ExampleToken(
            surface = px.surfaceSubset(from, to)
          )
        } else {
          ExampleToken(
            surface = p.surface,
            tags = p.tags.map(t => t.key -> t.value).toMap,
            unit = p.node
          )
        }

        tokens += token
      }
    }

    def make(): TokenSpan = {
      TokenSpan(
        index = idx,
        tokens = tokens,
        annotations = annotations
      )
    }
  }

  class MergeImpl(uid: ObjId, eplaced: Float) {
    private var inMerge = false
    private val blocks = new ArrayBuffer[PlannedBlock]()
    private val parts = new ArrayBuffer[PlannedPart]()

    private var annotations: Seq[Annotation] = Nil
    private var spans: ArrayBuffer[TokenSpanBuilder] = ArrayBuffer.empty

    private val result = new ArrayBuffer[SentenceBlock]()
    private var mergeFrom = 0
    private var mergeTo = 0

    def flush(): Unit = {
      if (blocks.isEmpty || parts.isEmpty) {
        return
      }

      annotations = Nil
      spans = ArrayBuffer.empty

      for (b <- blocks) {
        if (b.start < mergeFrom || b.end > mergeTo) {
          assert(b.noTags)
          handleBlock(b.subBlockNoTags(mergeFrom, mergeTo))
        } else {
          handleBlock(b.block)
        }
      }

      result += performMerge()

      blocks.clear()
      parts.clear()
      spans.clear()
      inMerge = false
    }

    def annotate(idx: Int): Seq[Annotation] = {
      val nonCur = annotations.filter(_.annotatorId != uid)
      val newAnn = Annotation(
        annotatorId = uid,
        value = idx.toString,
        timestamp = Some(Timestamps.now),
        duration = eplaced
      )

      nonCur :+ newAnn
    }

    def performMerge(): SentenceBlock = {
      // 1. check if a span already exist for the selected nodes.
      // this will be used e.g. when doing no-op edit-accept
      val bldr = spans.find(_.isFullMatch(parts))
      if (bldr.isDefined) {
        // annotate it and do not bother with actual merging
        return SentenceBlock(
          offset = mergeFrom,
          spans = spans.map(_.make()),
          annotations = annotate(bldr.get.idx)
        )
      }

      // 2. ok, there are no full matches
      // find variants not annotated by users except current one
      val otherAnnotations =
        annotations.filter(_.annotatorId != uid).flatMap(a => YInt.unapply(a.value)).toSet
      val filtered = spans.filter(s => !otherAnnotations.contains(s.idx))

      val tgtBldr = if (filtered.isEmpty) {
        // no good, need to add new sequence
        val newBldr = new TokenSpanBuilder(spans.length, Nil)
        spans += newBldr
        newBldr
      } else {
        // replace the most compatible sequence with details
        val newBldr = filtered.minBy(_.mismatch(parts))
        newBldr.clearTokens()
        newBldr
      }

      tgtBldr.appendTokens(parts, mergeFrom, mergeTo)

      SentenceBlock(
        offset = mergeFrom,
        spans = spans.map(_.make()),
        annotations = annotate(tgtBldr.idx)
      )
    }

    def handleBlock(block: SentenceBlock): Unit = {
      // step1: prepare and explode prefixes
      if (spans.isEmpty) {
        spans = new ArrayBuffer(block.spans.size)
        for (s <- block.spans) {
          spans += new TokenSpanBuilder(s.index, s.annotations)
        }
      }

      //step2: make number of spans equal
      if (spans.length != block.spans.length) {
        var curLen = spans.length
        val targetLen = block.spans.length

        while (curLen < targetLen) {
          spans += spans.head.contentClone(curLen)
          curLen += 1
        }
      }

      //step3: merge each span
      val it1 = spans.iterator
      val it2 = block.spans.iterator
      while (it1.hasNext && it2.hasNext) {
        val bldr = it1.next()
        val span = it2.next()
        bldr.appendSpan(span)
      }

      //step3.5: make sure that there are at least 2 options
      while (spans.length < 2) {
        spans += spans.head.contentClone(spans.length)
      }

      //step4: merge annotations
      annotations = mergeAnnotations(annotations, block.annotations)
    }

    private def appendParts(block: PlannedBlock, part: PlannedPart): Unit = {
      if (blocks.isEmpty || (blocks.last.ne(block))) {
        blocks += block
      }

      if (parts.isEmpty || (parts.last.ne(part))) {
        parts += part
      }
    }

    def feed(block: PlannedBlock, part: PlannedPart, from: Int, to: Int): Unit = {
      val noTags = block.noTags && part.noTags

      (inMerge, noTags) match {
        case (true, false) =>
          mergeTo = to
          appendParts(block, part)
        case (false, true) => result += block.subBlockNoTags(from, to)
        case (true, true) =>
          flush()
          result += block.subBlockNoTags(from, to)
        case (false, false) =>
          inMerge = true
          mergeFrom = from
          mergeTo = to
          appendParts(block, part)
      }
    }

    def resultFor(sent: Sentence): Sentence = {
      sent.copy(
        blocks = result
      )
    }
  }

  case class PlannedBlock(block: SentenceBlock, start: Int) {
    val noTags: Boolean = MergeSentence.noTags(block)
    val length: Int = block.spans.head.tokens.map(_.surface.length).sum
    def end: Int = start + length

    def subBlockNoTags(from: Int, to: Int): SentenceBlock = {
      if (from <= start && to >= end) {
        return block
      }

      val data = block.spans.head.tokens

      val res = new ArrayBuffer[ExampleToken]()

      var subStart = start

      val iter = data.iterator
      while (iter.hasNext) {
        val n = iter.next()

        val subEnd = subStart + n.surface.length

        if (subEnd <= from || subStart >= to) {
          // continue
        } else if (from <= subStart && subEnd <= to) {
          res += n
        } else {
          val xs = Math.max(from - subStart, 0)
          val xe = n.surface.length + Math.min(to - subEnd, 0)
          val surf = n.surface.substring(xs, xe)
          res += ExampleToken(surface = surf)
        }

        subStart = subEnd
      }

      block.copy(offset = from, spans = TokenSpan(tokens = res) :: Nil)
    }
  }

  case class PlannedPart(part: EditableSentencePart, start: Int) {
    def noTags: Boolean = MergeSentence.noTags(part)
    def length: Int = part.surface.length
    def end: Int = start + length
    def surfaceSubset(from: Int, to: Int): String = {
      val xs = Math.max(0, from - start)
      val xe = length - Math.max(0, end - to)
      part.surface.substring(xs, xe)
    }
  }

  class MergePlanItem(var from: Int = 0) {
    var to = 0
    val blocks = new ArrayBuffer[PlannedBlock]()
    val parts = new ArrayBuffer[PlannedPart]()

    def nonEmpty: Boolean = blocks.nonEmpty && parts.nonEmpty
  }

  def modBlocks(blocks: Seq[SentenceBlock]): Seq[PlannedBlock] = {
    var start = 0
    val bldr = new ArrayBuffer[PlannedBlock]()
    val iter = blocks.iterator

    while (iter.hasNext) {
      val pb = PlannedBlock(iter.next(), start)
      bldr += pb
      start += pb.length
    }

    bldr
  }

  def modParts(blocks: Seq[EditableSentencePart]): Seq[PlannedPart] = {
    var start = 0
    val bldr = new ArrayBuffer[PlannedPart]()
    val iter = blocks.iterator

    while (iter.hasNext) {
      val pb = PlannedPart(iter.next(), start)
      bldr += pb
      start += pb.length
    }

    bldr
  }

  def merge(base: Sentence, edits: EditableSentence, uid: ObjId, eplaced: Float): Sentence = {
    val blockIter = modBlocks(base.blocks).iterator.buffered
    val partIter = modParts(edits.parts).iterator.buffered

    var blockStart = 0
    var partStart = 0
    var mergeStart = 0

    val impl = new MergeImpl(uid, eplaced)

    while (blockIter.hasNext || partIter.hasNext) {
      val nextBlock = blockIter.head
      val nextPart = partIter.head
      val blockEnd = blockStart + nextBlock.length
      val partEnd = partStart + nextPart.length

      if (blockEnd == partEnd) {
        impl.feed(nextBlock, nextPart, mergeStart, blockEnd)
        mergeStart = blockEnd

        blockIter.next()
        blockStart = blockEnd
        partIter.next()
        partStart = partEnd
      } else if (blockEnd > partEnd) {
        impl.feed(nextBlock, nextPart, mergeStart, partEnd)

        mergeStart = partEnd
        partIter.next()
        partStart = partEnd
      } else {
        impl.feed(nextBlock, nextPart, mergeStart, blockEnd)
        mergeStart = blockEnd
        blockIter.next()
        blockStart = blockEnd
      }
    }

    impl.flush()
    impl.resultFor(base)
  }
}
