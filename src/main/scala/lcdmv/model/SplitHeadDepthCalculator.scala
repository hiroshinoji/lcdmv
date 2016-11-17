package lcdmv.model

import lcdmv.data.Sentence

/** Manage the condition of when to increase the stack depth
  *
  * WARNING: if no stack depth bound is given (maxDepth = 0 in the chart), UnlimitedDepthCalculator should be used. Otherwise the program crushes. This consistency should be checked by the user.
  */
trait SplitHeadDepthCalculator {

  /** maxChunkSize = 1 means it allows chunk of length = 1 at the deepest position of center-embedding
    */
  val maxChunkSize: Int

  protected def chunkSize(begin: Int, end: Int): Int = end - begin

  /** Return discrete values between [0, maxChunkLength]
    *
    * if max = 1, only 0 span (one word span) is allowed
    * but m = 1 (more than two words span) is also traced in the chart as the case of increasing depth (abstracts m = 2,3,... cases)
    */
  // def leftChunkSize(leftFullSpanSize: Int): Int = Math.min(leftFullSpanSize, maxChunkSize)
  def leftChunkSize(begin: Int, end: Int): Int = Math.min(chunkSize(begin, end), maxChunkSize)

  // this method should be modified appropriately in function word based contraints
  def possibleLeftCompSegments(left: Int, right: Int, m: Int) =
    if (m == maxChunkSize) left to right - 1 - m
    else Seq(right - 1 - m)

  // this method should be modified appropriately in function word based contraints
  def possibleLeftChunkSizes(left: Int, right: Int) =
    // 0 to leftChunkSize(left, right - 1)
    0 to leftChunkSize(left + 1, right)

  protected def compDepthDiff(leftSpanSize: Int, rightSpanSize: Int): Int

  def compDepthDiff(leftSpanSize: Int, rightBegin: Int, rightEnd: Int): Int = compDepthDiff(leftSpanSize, chunkSize(rightBegin, rightEnd))

  /** Used for right-comp (in optimal order PCFG) in which left span is always empty
    */
  def rightCompDepthDiff(begin: Int, end: Int) = compDepthDiff(0, begin, end)
}

/** Unlimited calculator also cares whether the size of leftSpan is greater than 1 or not.
  *
  * This information is necessary in calculation in LeafCondCalculator.
  */
case object UnlimitedDepthCalculator extends SplitHeadDepthCalculator {
  val maxChunkSize = 1

  def compDepthDiff(leftSpanSize: Int, rightSpanSize: Int) = 0
}

trait LimitedDepthCalcBase extends SplitHeadDepthCalculator {

  require(maxChunkSize > 0)

  def compDepthDiff(leftSpanSize: Int, rightSpanSize: Int) =
    if (leftSpanSize + rightSpanSize >= maxChunkSize) 1 else 0
}

/** maxChunkSize = 1 recovers the calculator of strict center-embeddedness of the structures
  */
case class LimitedDepthCalculator(val maxChunkSize: Int) extends LimitedDepthCalcBase

case class LimitedDepthWithCheckCalculater(val maxChunkSize: Int, sentence: Sentence)
    extends LimitedDepthCalcBase {

  val contentBits: Array[Boolean] = (0 until sentence.size).map(!sentence.pos(_).isFunc).toArray

  val chunkSizes = Array.tabulate(sentence.size, sentence.size) { (i, j) =>
    if (i < j) _chunkSize(i, j)
    else 0
  }

  private def _chunkSize(begin: Int, end: Int): Int = {
    var cnt = 0
    for (i <- begin to end) {
      if (contentBits(i)) cnt += 1
    }
    Math.max(0, cnt - 1)
  }

  /** c c c c => 3
    * c f f c => 1
    * c c f f => 1
    * c f f f => 0
    * f f f f => 0
    *
    * one content word => seems one word => 0
    * zero content word => seems one word => 0
    *
    * The basic idea is if a chunk contains at least one content word, that chunk is regarded as one word.
    */
  override def chunkSize(begin: Int, end: Int): Int = chunkSizes(begin)(end)

  override def possibleLeftCompSegments(left: Int, right: Int, m: Int) = {

    assert(right - left > m)

    val startNumContent = if (m == 0) 0 else m + 1
    val endNumContent = m + 2

    var start = right
    var end = right

    var cnt = 0
    for (ri <- 0 to (right - left)) {
      val i = right - ri
      if (contentBits(i)) {
        cnt += 1
        if (cnt == startNumContent && i < start) start = i
        if (cnt == endNumContent && i < end) end = i
      }
    }

    if (cnt < startNumContent) Seq() else {
      if (end < right && m < maxChunkSize) end = end + 1
      else end = left + 1

      end - 1 to start - 1
    }
  }

  // this method should be modified appropriately in function word based contraints
  // override def possibleLeftChunkSizes(left: Int, right: Int) = {
    // val numContents = leftChunkSize(left + 1, right)
    // var maxLength = -1
    // var cnt = 0

    // while (maxLength < (right - left) && cnt <= (maxChunkSize + 1)) {
    //   maxLength += 1
    //   if (contentBits(right - maxLength)) cnt += 1
    // }
    // 0 to (maxLength - 1)
  // }
}
