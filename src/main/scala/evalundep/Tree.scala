package evalundep

case class Tree[+T](label: T, children: Seq[Tree[T]]) {
  lazy val spanSize: Int = if (children.isEmpty) 1 else (children.map(_.spanSize)).sum

  // if conv returns None, that node is deleted
  def transformTopdown[E](conv: Tree[T]=>Option[E]): Option[Tree[E]] = {
    conv(this) flatMap { newLabel =>
      val newChildren = children flatMap { _.transformTopdown(conv) }
      if (newChildren.isEmpty && !children.isEmpty) None
      else Some(Tree(newLabel, newChildren))
    }
  }

  // assuming replacing some subtree is entirely replaced with another tree
  def transform[A>:T](conv: Tree[T]=>Option[Tree[A]]): Tree[A] = {
    conv(this) getOrElse { Tree(label, children map (_.transform(conv))) }
    // val newChildren =
    // conv(this)
  }

  def isPreterminal = children.size == 1 && children(0).isTerminal

  def isTerminal = children.isEmpty

  def isFlatSubtree = !children.isEmpty && children.forall(_.isPreterminal)

  def transformBottomUp[E](conv: Tree[T]=>Option[E]): Option[Tree[E]] = {
    val newChildren: Seq[Tree[E]] = children flatMap { _.transformBottomUp(conv) }
    conv(this) map { newLabel => Tree(newLabel, newChildren) }
  }

  override def toString = "(" + toStringHelper + ")"
    // if (isTerminal) label + ""
    // else "(" + label + " " + children.mkString(" ")  + ")"
  def toStringHelper: String =
    if (isTerminal) label + ""
    else if (isPreterminal) label + " " + children(0).toStringHelper
    else label + " " + children.mkString(" ")

  def toBracketTree(): Tree[(Int, Int)] = toBracketTreeHelper(0) { case (l, b) => b }
  def zipWithBracket(): Tree[(T, (Int, Int))] = toBracketTreeHelper(0) { case (l, b) => (l, b) }

  private def toBracketTreeHelper[A](offset: Int)(labelGen: (T, (Int, Int)) => A): Tree[A] =
    if (isTerminal) Tree.terminal(labelGen(label, (offset, offset)))
    else {
      val spanSizes = children.map(_.spanSize)
      val offsets = spanSizes.scanLeft(offset)(_ + _)
      val bracketedChildren = children.zip(offsets) map { case (c, o) => c.toBracketTreeHelper(o)(labelGen) }

      val end = offset + spanSize - 1

      // Tree(labelGen(label, (offset, offsets(offsets.size - 2))), bracketedChildren)
      Tree(labelGen(label, (offset, end)), bracketedChildren)
    }

  def toLabelSeq: Seq[T] = label +: children.flatMap(_.toLabelSeq)

  def toSeq: Seq[Tree[T]] = this +: children.toSeq

  def brackets: Set[(Int, Int)] = toBracketTree().toLabelSeq.toSet
}

object Tree {

  def terminal[W](w: W) = Tree(w, Nil)
  def preTerminal(token: Token) = Tree(token.pos, terminal(token.word) :: Nil)

  def removePuncs(tree: Tree[String]): Option[Tree[String]] =
    tree.transformTopdown { t =>
      if (t.isPreterminal && POSTag.isPunc(t.label)) None

      else Some(t.label)
    }

  def fromTokenDepTree(depTree: DepTree[Token]) = {
    val headSeq = depTree.headSeq
    val yields = depTree.yields
    val root = headSeq.indexOf(-1)

    def subTree(headIdx: Int): Tree[String] = {

      def isRoot = headIdx == root

      val leftChildren = (0 until headIdx).filter(headSeq(_) == headIdx)
      val rightChildren = (headIdx + 1 until yields.size).filter(headSeq(_) == headIdx)

      (leftChildren, rightChildren) match {
        case (Seq(), Seq()) =>
          val token = yields(headIdx)
          if (isRoot) Tree("TOP", preTerminal(token) :: Nil) else preTerminal(token)
        case _ =>
          def expand(childIdxs: Seq[Int]) = childIdxs.map(subTree(_))
          val children: Seq[Tree[String]] =
            (expand(leftChildren) :+ preTerminal(yields(headIdx))) ++ expand(rightChildren)
          val label = if (isRoot) "TOP" else "X"
          Tree(label, children)
      }
    }

    subTree(root)
  }

  def flattenToAdjustToGold[T](predTree: Tree[T], goldTree: Tree[T]): Tree[T] = {
    val bracketedGold = goldTree.zipWithBracket
    val bracketedPred = predTree.zipWithBracket

    val flatSubtreeMap = bracketedGold.toSeq.collect {
      case t @ Tree((_, span), _) if t.isFlatSubtree => span -> t
    }.toMap

    bracketedPred.transform { case tree @ Tree((_, span), _) =>
      flatSubtreeMap.get(span)
    }.transformTopdown { case tree => Some(tree.label._1) }.get
  }

  // case class BracketScore(prec: Double, recall: Double, f1: Dobule, ncprec: Dobule, unf1: Dobule)

  // def bracketScores[A,B](goldTrees: Seq[Tree[A]], predTrees: Seq[Tree[B]]) = {

  //   val goldBrackets = goldTrees.map(_.brackets)
  //   val predBrackets = predTrees.map(_.brackets)

  //   assert(goldBrackets.size == predBrackets.size)

  //   val numIntersectBrackets = predBrackets.zip(goldBrackets).map { case(p, g) =>
  //     val size = p.spanSize
  //     p.count { case (begin, end) => (begin != end && (begin == 0 && end == size) && g.contains((begin, end))) }
  //   }.map(_.size).sum

  //   val numUncorssedPredictedBrackets = predBrackets.zip(goldBrackets).map { case (p, g) =>
  //     val size = p.spanSize
  //     p.count { case (begin, end) =>
  //       def crossed = g.exists { case (gBegin, gEnd) =>
  //         (gBegin < begin && begin <= gEnd && gEnd < end) || // do not check when gBegin == begin
  //         (begin < gBegin && end < gEnd)
  //       }
  //       begin != end && (begin == 0 && end == size) && !crossed
  //     }
  //   }

  // }
}
