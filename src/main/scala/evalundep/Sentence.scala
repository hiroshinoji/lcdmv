package evalundep

case class Token(word: String, pos: String) {
  def isPunc = POSTag.isPunc(pos)
  def isNoun = POSTag.isNoun(pos)
  def isVerb = POSTag.isVerb(pos)
  def isNV = isNoun || isVerb

  def isPrep = POSTag.isPrep(pos)
}

trait SentenceLike {
  def size: Int
}

trait Sentence[W, P] {
  def size: Int
  def word(i: Int): W
  def pos(i: Int): P
}

case class TaggedSentence[W, P](
  wordSeq: IndexedSeq[W],
  posSeq: IndexedSeq[P]
) extends Sentence[W, P] {
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
  def feats: IndexedSeq[String]
  def head: IndexedSeq[Int]
  def label: IndexedSeq[String]

  def size = word.size

  def headSeq = head

  def isPunc(i: Int) = POSTag.isPunc(pos(i))

  // this i is 0-based
  def children(i: Int): Seq[Int] = (leftChildren(i) ++ rightChildren(i))
  def leftChildren(i: Int) = (0 until i).filter(head(_) == i + 1)
  def rightChildren(i: Int) = (i + 1 until size).filter(head(_) == i + 1)

  // def containsArc()
  // def containsChild(parent: Int): Boolean = children().exists {
  //   case j if isPunc(j) => containsChild(j)
  //   case j => i == j
  // }

  // this i is 0-based
  def grandparent(i: Int): Option[Int] = parent(i) flatMap(parent(_))
  def parent(i: Int): Option[Int] = if (i == -1) None else {
    val pidx = head(i) - 1
    if (pidx == -1) Some(-1)
    else if (POSTag.isPunc(pos(pidx))) parent(pidx)
    else Some(pidx)
  }

  override def toString = (0 until size).map { i =>
    s"${i+1}\t${word(i)}\t${lemma(i)}\t${cpos(i)}\t${pos(i)}\t${feats(i)}\t${head(i)}\t${label(i)}"
  }.mkString("\n")

  def countDirected(pred: CoNLLSentence): (Int, Int) = {
    if (size != pred.size) (0, unpuncIdxs.size)
    else {
    // assert(size == pred.size)
      val correct = unpuncIdxs.count { i => parent(i) == pred.parent(i) } // head(i) == pred.head(i) }
        (correct, unpuncIdxs.size)
    }
  }
  def countUndirected(pred: CoNLLSentence): (Int, Int) = {
    if (size != pred.size) (0, unpuncIdxs.size)
    else {
      val correct = unpuncIdxs.count { i =>
        parent(i) == pred.parent(i) ||
        pred.parent(i).flatMap { parent(_).map(_ == i) }.getOrElse(false)
        // children(i).contains(pred.head(i) - 1)
      }
      (correct, unpuncIdxs.size)
    }
  }
  def countNED(pred: CoNLLSentence): (Int, Int) = {
    if (size != pred.size) (0, unpuncIdxs.size)
    else {
      val correct = unpuncIdxs.count { i =>
        parent(i) == pred.parent(i) ||
        pred.parent(i).flatMap { parent(_).map(_ == i) }.getOrElse(false) ||
        pred.parent(i).flatMap { pp => grandparent(i).map(_ == pp) }.getOrElse(false)
        // grandparent(i).map(_ == pred.head(i) - 1).getOrElse(false)
      }
      (correct, unpuncIdxs.size)
    }
  }
  // this is not correct for multi-root trees
  def root(pred: CoNLLSentence): Boolean = head.indexOf(0) == pred.head.indexOf(0)
  lazy val unpuncIdxs = (0 until size) filter { i => !POSTag.isPunc(pos(i)) }
}

case class CoNLLXSentence(
  val word: IndexedSeq[String],
  val lemma: IndexedSeq[String],
  val cpos: IndexedSeq[String],
  val pos: IndexedSeq[String],
  // val upos: IndexedSeq[String],
  val feats: IndexedSeq[String],
  val head: IndexedSeq[Int],
  val label: IndexedSeq[String]) extends CoNLLSentence

object CoNLLSentence {

  /** Assume each label is: word/pos
    */
  def fromIndexedStringDepTree(tree: DepTree[(String, Int)]) = {
    val yields: IndexedSeq[(String, Int)] = tree.mapWithParent { // label and head
      case ((label, idx), p) =>
        val pidx = p.map(_._2).getOrElse(-1)
        ((label, pidx), idx)
    }.yields.sortBy(_._2).map(_._1)

    if (yields(0)._1 == "root/root") {
      val rootRemoved = yields.drop(1)
      fromIndexedDepTreeHelper(
        rootRemoved.map(_._1),
        rootRemoved.map(_._2),
        standardExtractor)
    } else {
      fromIndexedDepTreeHelper(
        yields.map(_._1),
        yields.map(_._2 + 1), // implicitly consider dummy root
        standardExtractor)
    }
  }

  val standardExtractor: (String=>(String,String,String)) = token => {
    val slash = token.lastIndexOf('/')
    val word = token.take(slash)
    val pos = token.drop(slash + 1)
    (word, pos, pos)
  }

  def fromIndexedDepTreeHelper[T](tokens: IndexedSeq[T], heads: IndexedSeq[Int], toWordPos: (T=>(String, String, String))) = {
    val tokenSeq = tokens.map(toWordPos(_))
    val emptySeq = tokenSeq.map(_=>"_")

    CoNLLXSentence(
      tokenSeq.map(_._1),
      emptySeq,
      tokenSeq.map(_._2),
      tokenSeq.map(_._3),
      emptySeq,
      heads,
      emptySeq)
  }

  // /** toWordPoS: label => (word, cpos, pos)
  //   */
  // def fromIndexedDepTree[T](tree: DepTree[(T, Int)])(toWordPoS: (T=>(String, String, String))) = {
  //   val headSeq = tree.mapWithParent {
  //     case ((_, idx), None) => 0 // head
  //     case ((_, idx), Some((_, parentIdx))) => parentIdx + 1
  //   }.yields
  //   val tokenSeq = tree.yields.map { case (label, idx) => toWordPoS(label) }
  //   val emptySeq = tokenSeq.map(_=>"_")

  //   CoNLLXSentence(
  //     tokenSeq.map(_._1),
  //     emptySeq,
  //     tokenSeq.map(_._2),
  //     tokenSeq.map(_._3),
  //     emptySeq,
  //     headSeq,
  //     emptySeq)
  // }

  def readCoNLLFile(input: IndexedSeq[String]): IndexedSeq[CoNLLXSentence] = {
    val segmentIdxs = 0 +: (0 until input.size).collect { case i if input(i).trim.isEmpty => i } :+ input.size
    // val idxs = (0 until input.size)
    // val segmentIdxs = idxs.scanLeft(0) { case (prev, _) =>
    //   input.indexWhere(_.isEmpty, prev + 1)
    // }

    // val segmentIdxs = 0 +: input.zipWithIndex.filter(_._1.trim.isEmpty).map(_._2) :+ input.size
    segmentIdxs.sliding(2).flatMap { case Seq(begin, end) =>
      val tokens = (begin until end).map(input(_)).filter(!_.trim.isEmpty).map(_.split("\t"))
      tokens match {
        case Seq() => None
        case _ => Some(CoNLLXSentence(tokens.map(_(1)),
          tokens.map(_(2)),
          tokens.map(_(3)),
          tokens.map(_(4)),
          tokens.map(_(5)),
          tokens.map(_(6).toInt),
          tokens.map(_(7))))
      }
    }.toIndexedSeq
  }
}
