package lcdmv.data

trait SentenceLike {
  def size: Int
}

trait Sentence {
  def size: Int
  def word(i: Int): Word
  def pos(i: Int): PoS

  def wordContains(f: Word=>Boolean): Boolean = (0 until size).exists { i => f(word(i)) }
  def posContains(f: PoS=>Boolean): Boolean = (0 until size).exists { i => f(pos(i)) }
}

object Sentence {
  def empty(): Sentence = new TaggedSentence(IndexedSeq(), IndexedSeq())
}

case class TaggedSentence[W<:Word, P<:PoS](
  wordSeq: IndexedSeq[W],
  posSeq: IndexedSeq[P]
) extends Sentence {
  assert(wordSeq.size == posSeq.size)

  override def word(i: Int) = wordSeq(i)
  override def pos(i: Int) = posSeq(i)
  override def size = wordSeq.size
}

trait CoNLLSentence extends SentenceLike {

  def word: IndexedSeq[String]
  def lemma: IndexedSeq[String]
  def cpos: IndexedSeq[String]
  def pos: IndexedSeq[String]
  def upos: IndexedSeq[String]
  def feats: IndexedSeq[String]
  def head: IndexedSeq[Int] // 0-based (head of root = -1)
  def label: IndexedSeq[String]

  def size = word.size

  // def wordSeq(dict: Dictionary) = word.map { w => dict.word(w) }
  // def posSeq(dict: Dictionary) = pos.map { p => dict.posOrCreate(p) }
  def headSeq = head

  /** Obsolute: use StringSentence.toTaggedSentence through toStringSentence
    */
  // def toTaggedSentence(dict: Dictionary) =
  //   TaggedSentence(wordSeq(dict), posSeq(dict))

  def toStringSentence = StringSentence(word, pos, upos)
  def toStringSentenceWithCPos = StringSentence(word, cpos, upos)
  def toStringSentenceWithUPos = StringSentence(word, upos, upos)

  def toDepArcs = NormalDepArcs(headSeq)

  def toCoNLLXString(arcs: Option[DepArcs]) = {
    def head(i: Int) = arcs map(_.head(i) + 1) getOrElse(0)
    (0 until size).map { i =>
      s"${i+1}\t${word(i)}\t${lemma(i)}\t${cpos(i)}\t${pos(i)}\t${feats(i)}\t${head(i)}\t${label(i)}"
    }.mkString("\n")
  }

  def toPascalCoNLLString(arcs: Option[DepArcs]) = {
    def head(i: Int) = arcs map(_.head(i) + 1) getOrElse(0)
    (0 until size).map { i =>
      s"${i+1}\t${word(i)}\t${lemma(i)}\t${cpos(i)}\t${pos(i)}\t${upos(i)}\t${feats(i)}\t${head(i)}\t${label(i)}"
    }.mkString("\n")
  }
}

case class CoNLLXSentence(
  val word: IndexedSeq[String],
  val lemma: IndexedSeq[String],
  val cpos: IndexedSeq[String],
  val pos: IndexedSeq[String],
  val upos: IndexedSeq[String],
  val feats: IndexedSeq[String],
  val head: IndexedSeq[Int],
  val label: IndexedSeq[String]) extends CoNLLSentence {


}

object CoNLLXSentence {
  def fromLines(lines: IndexedSeq[String]): CoNLLXSentence = {
    val segmented = lines map { _.split("\t").toSeq }
    val cpos = segmented map (_(3))
    val pos = segmented.map (_(4)) // .map { p =>
    //   if (p.contains('_')) p.take(p.indexOf('_')) else p
    // }
    val blankUpos = pos map (_=>"_")

    CoNLLXSentence(
      segmented map(_(1)),
      segmented map(_(2)),
      cpos,
      if (pos.count(_=="_") == pos.size) cpos else pos,
      cpos, // if upos is not given, assume cpos is upos (some corpos, e.g., google universal treebank follows this)
      segmented map(_(5)),
      segmented map(_(6).toInt - 1), // WARNING: We don't use any dummy on the begin or end of a sentence
      segmented map(_(7)))
  }

  def fromUniversalLines(lines: IndexedSeq[String]): CoNLLXSentence = {
    val segmented = lines map { _.split("\t").toSeq }

    def safeStringToInt(str: String): Option[Int] = {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toInt
    }

    def safeHead(i: String) = safeStringToInt(i) getOrElse (0)

    CoNLLXSentence(
      segmented map(_(1)),
      segmented map(_(2)),
      segmented map(_(3)), // cpos
      segmented.map(_(4)), // .map { p =>
      //   if (p.contains('_')) p.take(p.indexOf('_')) else p
      // }, // pos
      segmented map(_(5)),
      segmented map(_(6)), // 5 is universal pos => skip
      segmented map(w=>safeHead(w(7)) - 1), // WARNING: We don't use any dummy on the begin or end of a sentence
      segmented map(_(8)))
  }
}

case class StringSentence(
  word: IndexedSeq[String],
  pos: IndexedSeq[String],
  upos: IndexedSeq[String]) extends SentenceLike {

  def size = word.size

  def wordSeq(dict: Dictionary) = word.map(dict.word(_))
  def posSeq(dict: Dictionary) = pos.map(dict.pos(_))

  def toTaggedSentence(dict: Dictionary) =
    TaggedSentence(wordSeq(dict), posSeq(dict))
  def toStringSentence = this
  def toDepArcs = NormalDepArcs(Array.fill(word.size)(-1))
}

object StringSentence {
  def apply(word: IndexedSeq[String], pos: IndexedSeq[String]): StringSentence = StringSentence(word, pos, pos.map(_=>"_"))
}
