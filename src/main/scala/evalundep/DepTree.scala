package evalundep

// end exclusive
case class SpanLabel[+T](v: T, head: Int, begin: Int, end: Int)

case class DepTree[+T](val label: T, val leftChildren: IndexedSeq[DepTree[T]], val rightChildren: IndexedSeq[DepTree[T]]) {

  lazy val spanSize: Int = 1 + (leftChildren.map { _.spanSize }).sum + (rightChildren.map { _.spanSize }).sum

  def foreachNode[U](f:DepTree[T]=>U): Unit = {
    f(this)
    leftChildren.foreach { _.foreachNode(f) }
    rightChildren.foreach { _.foreachNode(f) }
  }
  def toSeq: Seq[DepTree[T]] = leftChildren.flatMap(_.toSeq) ++ (this +: rightChildren.flatMap(_.toSeq))

  def isLeaf = leftChildren.isEmpty && rightChildren.isEmpty

  def toIndexedTree(offset: Int = 0): DepTree[(T, Int)] = if (isLeaf) DepTree.terminal((label, offset)) else {

    // return indexed children and the end offset (end index of child span)
    def toIndexedChildren(children: IndexedSeq[DepTree[T]], startOffset: Int): (IndexedSeq[DepTree[(T, Int)]], Int) = {
      val spanSizes = children map (_.spanSize)
      val offsets = spanSizes.scanLeft(startOffset) { _ + _ }
      assert(offsets.size == spanSizes.size + 1)
      (children.zip(offsets) map { case (c, o) => c.toIndexedTree(o) }, offsets.last)
    }
    val (indexedLeft, leftOffset) = toIndexedChildren(leftChildren, offset)
    val (indexedRight, finalOffset) = toIndexedChildren(rightChildren, leftOffset + 1)

    DepTree.nonterminal((label, leftOffset), indexedLeft, indexedRight)
  }

  def toSpanTree(offset: Int = 0): DepTree[SpanLabel[T]] = if (isLeaf) DepTree.terminal(SpanLabel(label, offset, offset, offset + 1)) else {

    def toSpanChildren(children: IndexedSeq[DepTree[T]], startOffset: Int): (IndexedSeq[DepTree[SpanLabel[T]]], Int) = {
      val spanSizes = children map (_.spanSize)
      val offsets = spanSizes.scanLeft(startOffset) { _ + _ }
      assert(offsets.size == spanSizes.size + 1)
      (children.zip(offsets) map { case (c, o) => c.toSpanTree(o) }, offsets.last)
    }
    val (spanLeft, leftOffset) = toSpanChildren(leftChildren, offset)
    val (spanRight, finalOffset) = toSpanChildren(rightChildren, leftOffset + 1)

    DepTree.nonterminal(SpanLabel(label, leftOffset, offset, offset + spanSize), spanLeft, spanRight)
  }

  def headSeq: IndexedSeq[Int] = {
    val indexed = toIndexedTree()
    indexed.headSeqFromIndexed()
  }

  def headSeqFromIndexed[A]()(implicit a: T<:<(A,Int)): IndexedSeq[Int] = this.mapWithParent {
    case (_, None) => -1
    case (_, Some(p)) => p._2
  }.yields

  def map[E](conv: T=>E): DepTree[E] =
    DepTree(conv(label), leftChildren.map(_.map(conv)), rightChildren.map(_.map(conv)))

  def mapWithParent[E](conv: (T, Option[T])=>E): DepTree[E] =
    mapWithParentHelper(conv, None)

  // I don't fully understand why this lowr bound must be declared here while it is not necessary above but it works.
  private def mapWithParentHelper[A>:T, E](conv: (A, Option[A])=>E, parent: Option[A]): DepTree[E] =
    DepTree(conv(label, parent), leftChildren.map(_.mapWithParentHelper(conv, Some(label))), rightChildren.map(_.mapWithParentHelper(conv, Some(label))))

  def yields: IndexedSeq[T] = toSeq.map(_.label).toIndexedSeq

  override def toString = {
    val leftStr = childrenStr(leftChildren)
    val rightStr = childrenStr(rightChildren)

    val leftSpace = if (leftStr.isEmpty) "" else " "
    val rightSpace = if (rightStr.isEmpty) "" else " "
    "(" + leftStr + leftSpace + label + rightSpace + rightStr + ")"
  }
  private[this] def childrenStr(children: Seq[DepTree[T]]) = children.map(_ + "").mkString(" ")

  def toTokenizedTree(implicit a: T => collection.immutable.WrappedString) = this map { token =>
    val slash = token.lastIndexOf('/')
    val word = token.take(slash)
    val pos = token.drop(slash + 1)
    Token(word, pos)
  }

  // remove should be performed with preserving index perhaps given externally
  def removeAll[A](cond: T=>Boolean)(implicit a:T<:<(A, Int)): DepTree[(A, Int)] = {

    val removedTree = removeAllHelper(cond, true)

    // align index
    val idxMap = removedTree.map(_._2).yields.sortWith(_ < _).zipWithIndex.map {
      case (oldIdx, newIdx) => (oldIdx, newIdx)
    }.toMap

    removedTree.map(x => (x._1, idxMap(x._2)))

    // def collectChildren(children: IndexedSeq[DepTree[T]]): IndexedSeq[DepTree[T]] =
    //   children.flatMap {
    //     case c if cond(c.label) => collectChildren(c.leftChildren ++ c.rightChildren)
    //     case c => IndexedSeq(c)
    //   }

    // val newLeft = collectChildren(leftChildren).map(_.removeAll(cond, false))
    // val newRight = collectChildren(rightChildren).map(_.removeAll(cond, false))
  }
  private def removeAllHelper[A](cond: T=>Boolean, isRoot: Boolean)(implicit a:T<:<(A, Int)): DepTree[T] = {

    // the node that satisfies cond cannot be traversed
    // because that node is removed by its parent node
    if (cond(label)) assert(isRoot)

    def collectChildren(children: IndexedSeq[DepTree[T]])(): IndexedSeq[DepTree[T]] =
      children.flatMap {
        case c if cond(c.label) => collectChildren(c.leftChildren ++ c.rightChildren)
        case c => IndexedSeq(c)
      }
    val unsortedChildren: IndexedSeq[DepTree[T]] = collectChildren(leftChildren ++ rightChildren).map(_.removeAllHelper(cond, false))
    val newLeft = unsortedChildren.filter(_.label._2 < label._2)
    val newRight = unsortedChildren.filter(_.label._2 > label._2)
    assert(newLeft.size + newRight.size == unsortedChildren.size)

    if (isRoot && cond(label)) {
      val (modifiedLeftChildren, mostRight) = newRight.lastOption.map(c => (newLeft ++ newRight.dropRight(1), c)).getOrElse((newLeft.dropRight(1), newLeft.last))
      mostRight.copy(leftChildren = modifiedLeftChildren ++ mostRight.leftChildren)
    } else this.copy(leftChildren = newLeft, rightChildren = newRight)
  }

  // def removeAll(cond: T=>Boolean, isRoot: Boolean = true): DepTree[T] = {
  //   if (cond(label)) assert(isRoot)

  //   def collectChildren(children: IndexedSeq[DepTree[T]]): IndexedSeq[DepTree[T]] =
  //     children.flatMap {
  //       case c if cond(c.label) => collectChildren(c.leftChildren ++ c.rightChildren)
  //       case c => IndexedSeq(c)
  //     }

  //   val newLeft = collectChildren(leftChildren).map(_.removeAll(cond, false))
  //   val newRight = collectChildren(rightChildren).map(_.removeAll(cond, false))

  //   if (isRoot && cond(label)) {
  //     val (modifiedLeftChildren, mostRight) = newRight.lastOption.map(c => (newLeft ++ newRight.dropRight(1), c)).getOrElse((newLeft.dropRight(1), newLeft.last))
  //     mostRight.copy(leftChildren = modifiedLeftChildren ++ mostRight.leftChildren)
  //   } else this.copy(leftChildren = newLeft, rightChildren = newRight)
  // }
}

object DepTree {
  def terminal[T](label: T) = DepTree(label, IndexedSeq[DepTree[T]](), IndexedSeq[DepTree[T]]())
  // just alias to the constructor
  def nonterminal[T](label: T, leftChildren: IndexedSeq[DepTree[T]], rightChildren: IndexedSeq[DepTree[T]]) =
    DepTree(label, leftChildren, rightChildren)

  /** from e.g., '((I/PRP) am/VBP ((a/DT) scientist/NN) (./.))'
    */
  def fromBracketString(treeStr: String): DepTree[String] = {
    type T = DepTree[String]

    def nextPos(offset: Int): Int = treeStr.indexWhere(_ != ' ', offset)

    def findTree(offset: Int): (Int, T) = {
      val begin = nextPos(offset)
      assert(treeStr(begin) == '(')
      val next = nextPos(begin + 1)

      val (leftPos, leftChildren) = findChildren(next)

      val labelPos = nextPos(leftPos)
      val labelEndPos = treeStr.indexWhere(c => c == ' ' || c == ')', labelPos + 1)
      val label = treeStr.slice(labelPos, labelEndPos)

      val (rightPos, rightChildren) = findChildren(labelEndPos)
      val endPos = nextPos(rightPos)

      assert(treeStr(endPos) == ')') // corresponds to 'begin'
      (endPos + 1, nonterminal(label, leftChildren, rightChildren))
    }
    def findChildren(offset: Int): (Int, IndexedSeq[T]) = collectChildren(offset) match {
      case (childrenOffset, children) => (childrenOffset, children.toIndexedSeq.reverse)
    }
    def collectChildren(offset: Int, current: List[T] = List[T]()): (Int, List[T]) = {
      val begin = nextPos(offset)
      treeStr(begin) match {
        case '(' =>
          val (childOffset, nextChild) = findTree(begin)
          collectChildren(childOffset, nextChild :: current)
        case _ =>
          (begin, current)
      }
    }
    findTree(0)._2
  }

  def fromCoNLL(sentence: CoNLLSentence): DepTree[String] = {
    fromCoNLLHelper(
      sentence.word.zip(sentence.pos).map { case (w, p) => w+'/'+p },
      sentence.head.map(_ - 1),
      sentence.head.indexOf(0))
  }

  // this index may gives non-projective links
  def fromCoNLLWithDummy(sentence: CoNLLSentence): DepTree[(String, Int)] = {
    val word = "root" +: sentence.word
    val pos = "root" +: sentence.pos
    val head = -1 +: sentence.head

    val tokenSeq = word.zip(pos).zipWithIndex.map { case ((w, p), i) => (w+'/'+p, i) }

    fromCoNLLHelper(tokenSeq, head, 0)
  }

  private def fromCoNLLHelper[T](tokenSeq: IndexedSeq[T], headSeq: IndexedSeq[Int], root: Int) = {
    def subTree(headIdx: Int): DepTree[T] = {
      val label = tokenSeq(headIdx)
      val leftDeps = (0 until headIdx).filter(headSeq(_) == headIdx).map(subTree(_))
      val rightDeps = (headIdx + 1 until headSeq.size).filter(headSeq(_) == headIdx).map(subTree(_))

      nonterminal(label, leftDeps, rightDeps)
    }
    subTree(root)
  }

  // def create(sentence:Seq[Word], deps:Dependencies):DepTree[Word] = {
  //   def subTree(head:Int): DepTree[Word] = {
  //     val label = sentence(head)
  //     val leftDependents = deps.leftDependents(head).toList.map { subTree(_) }
  //     val rightDependents = deps.rightDependents(head).toList.map { subTree(_) }

  //     if (head == deps.root) {
  //       RootNode(label, leftDependents, rightDependents)
  //     } else (leftDependents, rightDependents) match {
  //       case (Nil, Nil) => UnIndexedLeafNode(label)
  //       case _ => UnIndexedIntermNode(label, leftDependents, rightDependents)
  //     }
  //   }
  //   subTree(deps.root)
  // }
}
