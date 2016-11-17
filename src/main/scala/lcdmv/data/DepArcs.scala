package lcdmv.data

/** Representantion of a set of dependency arcs in a sentence.
  */
trait DepArcs {
  def heads: Seq[Int]
  def root:Int
  def range:Seq[Int]

  def size = range.size

  def nonDummyRoots: Seq[Int] = range.filter(head(_) == root)

  def head(i:Int):Int = heads(i)
  def dependentsIn(i:Int, fromIdx:Int):Seq[Int] = (fromIdx until heads.size).filter { heads(_) == i }
  def leftDependents(i:Int):Seq[Int] = (0 until i).filter { heads(_) == i }
  def rightDependents(i:Int):Seq[Int] = (i+1 until heads.size).filter { heads(_) == i }
  def nextDependentIn(i:Int, fromIdx:Int): Option[Int] = (fromIdx until heads.size).find { heads(_) == i }

  def existsNonProjectiveLink = heads.zipWithIndex.exists { case (head, dep) =>
    val begin = math.min(head, dep) + 1
    val end = math.max(head, dep)
    (begin until end).exists { child => !isConnected(child, head) && !isConnected(child, dep) }
  }
  def existsCycle = heads.zipWithIndex.drop(1).exists { case (head, dep) => isConnected(head, dep) }

  def isProjective = !existsNonProjectiveLink

  /** Return whether child and parent are connected in the graph
    */
  def isConnected(child:Int, parent:Int):Boolean = {
    if (parent == child) true
    else if (child == -1) false
    else isConnected(head(child), parent)
  }

  def arity:Int = (0 until heads.size).map { dependentsIn(_, 0).size }.max

  def numCorrectHeads(arcs: DepArcs) = range.filter { i => heads(i) == arcs.heads(i) }.size

  def rightAncestors(i: Int): List[Int] = {
    def addAncestors(j: Int, l: List[Int]): List[Int] = head(j) match {
      case -1 => l
      case h if h == root => root :: l
      case h if j < h => addAncestors(h, h :: l)
      case _ => l
    }
    addAncestors(i, Nil).reverse
  }

  // /** This is used when inserting punctuation nodes for predicted arcs, which are assigned on a punctuation removed sentence.
  //   *
  //   * @param unremovedIdxs indices of tokens which was kept unremoved in original sentence
  //   *
  //   * e.g., when sentence = [a, b, ., ., c] where . = punctuation,
  //   * unremovedIdxs = [0, 1, 4].
  //   *
  //   * Maybe the current DepArc (sequence of heads) is [-1, 2, 0] (root->a; a->c; b<-c).
  //   * Then, this method returns new DepArc, which is [-1, 4, -1, -1, 0]; i.e., it assigns -1 for all punctuation (removed) tokens
  //   *
  //   */
  // def recoverRemovedArcs(unremovedIdxs: IndexedSeq[Int])
}

case class NormalDepArcs(override val heads: Seq[Int]) extends DepArcs {
  def root = -1
  def range = 0 until heads.size

  // if (rightDependents(root).size == 0) throw new MultipleOrNoRoots
}
