package evalundep

import scala.collection.mutable.HashSet
import scala.util.Random

class DepTreeConverter {

  trait Dir
  object Left extends Dir
  object Right extends Dir
  // val L = 0
  // val RIGHT = 1

  // to handle non-projective, every tree should be indexed externally
  def toFlippedTree[A, B](modifying: DepTree[(A, Int)], reference :DepTree[(B, Int)]) = {

    val headSeq = reference.headSeqFromIndexed
    // val indexed = modifying.toIndexedTree()

    flipRecursively(modifying, -1, headSeq) //.map(_._1)
    // val spanToHead = calcSpanToHead(reference)
    // val spaned: DepTree[SpanLabel[A]] = modifying.toSpanTree()
  }

  def flipRecursively[A](tree: DepTree[(A, Int)], parent: Int, headSeq: Seq[Int]): DepTree[(A, Int)] = {
    val leftChildren = tree.leftChildren
    val rightChildren = tree.rightChildren

    val currentHead = tree.label._2

    val parentOfCurrentHead = headSeq(currentHead)

    def candidateChildIdx(children: IndexedSeq[DepTree[(A, Int)]]): Option[Int] =
      children.zipWithIndex.find(_._1.label._2 == parentOfCurrentHead).map(_._2)

    def unchanged() = tree.copy(
      leftChildren = leftChildren.map { c => flipRecursively(c, currentHead, headSeq) },
      rightChildren = rightChildren.map { c => flipRecursively(c, currentHead, headSeq) })

    def flipChildren(children: IndexedSeq[DepTree[(A, Int)]], p: Int) =
      children.map { c => flipRecursively(c, p, headSeq) }

    def leftFlip(idx: Int) = {
      val newHeadTree = tree.leftChildren(idx)

      val newLeftChildren = flipChildren(tree.leftChildren.take(idx) ++ newHeadTree.leftChildren, parentOfCurrentHead)
      val remainingLeftChildren = flipChildren(tree.leftChildren.drop(idx + 1), currentHead)
      val remainingRightChildren = flipChildren(tree.rightChildren, currentHead)

      val newChild = tree.copy(
        leftChildren = remainingLeftChildren,
        rightChildren = remainingRightChildren)

      val newRightChildren = flipChildren(newHeadTree.rightChildren, parentOfCurrentHead) :+ newChild

      newHeadTree.copy(leftChildren = newLeftChildren, rightChildren = newRightChildren)
    }
    def rightFlip(idx: Int) = {
      val newHeadTree = tree.rightChildren(idx)

      val newRightChildren = flipChildren(newHeadTree.rightChildren ++ tree.rightChildren.drop(idx + 1), parentOfCurrentHead)
      val remainingRightChildren = flipChildren(tree.rightChildren.take(idx), currentHead)
      val remainingLeftChildren = flipChildren(tree.leftChildren, currentHead)

      val newChild = tree.copy(
        leftChildren = remainingLeftChildren,
        rightChildren = remainingRightChildren)

      val newLeftChildren = newChild +: flipChildren(newHeadTree.leftChildren, parentOfCurrentHead)

      newHeadTree.copy(leftChildren = newLeftChildren, rightChildren = newRightChildren)
    }

    if (parent == parentOfCurrentHead) unchanged
    else candidateChildIdx(leftChildren).map(i => leftFlip(i)).getOrElse {
      candidateChildIdx(rightChildren).map(i => rightFlip(i)).getOrElse(unchanged)
    }
  }

  type Node = DepTree[(Token, Int)]

  // find a path of comprises of verb or noun and PP.
  def injectNoiseOfPPAttachment(tree: DepTree[Token]): DepTree[Token] = {
    val indexed = tree.toIndexedTree()
    injectNoiseRecursively(indexed).map { case (t, i) => t }
  }
  private def injectNoiseRecursively(tree: Node): Node = {
    def isPrepRightChild = tree.rightChildren.lastOption.map(_.label._1.isPrep).getOrElse(false)

    val newTree = if (tree.label._1.isNV && isPrepInRightSpine(tree)) {
      val (rightSpine, parentIdx) = pathAndPrepParentIndex(tree)
      val prep = rightSpine(parentIdx).rightChildren.last
      assert(prep.label._1.isPrep)

      val cands = (0 until rightSpine.size).filter(i => rightSpine(i).label._1.isNV && i != parentIdx)

      if (cands.isEmpty) tree // do nothing
      else {
        val sample = cands(Random.nextInt(cands.size))
        swapChildOnPath(rightSpine, prep, parentIdx, sample)
      }

      // }
      //  match {
      //   case `parentIdx` =>
      //   case sampledIdx =>
      // }
    } else tree

    def injectToChildren(children: IndexedSeq[Node]) = children.map(c => injectNoiseRecursively(c))

    val newLeftChildren = injectToChildren(newTree.leftChildren)
    // val newRightChildren = if (!newTree.rightChildren.isEmpty) {
    //   injectToChildren(newTree.rightChildren.dropRight(1)) :+ newTree.rightChildren.last
    // } else newTree.rightChildren
    val newRightChildren = injectToChildren(newTree.rightChildren)
    newTree.copy(
      leftChildren = newLeftChildren,
      rightChildren = newRightChildren)
  }

  private def swapChildOnPath(path: IndexedSeq[Node], target: Node, oldIdx: Int, newIdx: Int) = {
    require(oldIdx != newIdx)
    // val deeper = Math.max(oldIdx, newIdx)

    def newNode(node: Node, idx: Int): Node = {
      val rightChildren = idx match {
        case `oldIdx` =>
          val r = node.rightChildren
          assert(r.last.label._2 == target.label._2)

          if (r.size > 1) r.dropRight(2) :+ newNode(r(r.size - 2), idx + 1)
          else r.dropRight(2)
        case `newIdx` =>
          val r = node.rightChildren

          if (!r.isEmpty) r.dropRight(1) ++ Seq(newNode(r.last, idx + 1), target)
          else IndexedSeq(target)
        case _ =>
          val r = node.rightChildren
          if (!r.isEmpty) r.dropRight(1) :+ newNode(r.last, idx + 1)
          else r
      }
      node.copy(rightChildren = rightChildren)
    }
    newNode(path(0), 0)
  }

  private def isPrepInRightSpine(tree: Node): Boolean = tree.rightChildren match {
    case Seq() => false
    case children if children.last.label._1.isPrep => true
    case children => isPrepInRightSpine(children.last)
  }

  private def pathAndPrepParentIndex(tree: Node): (IndexedSeq[Node], Int) = {
    require(!tree.label._1.isPrep)

    def addSpine(node: Node, idx: Int, parentIdx: Option[Int], lst: List[Node]): (Option[Int], List[Node]) =
      node.rightChildren match {
        case Seq() => (parentIdx, node :: lst)
        case children if children.last.label._1.isPrep && parentIdx == None =>
          //val pidx =Some(idx))

          if (children.size == 1) (Some(idx), node :: lst)
          else addSpine(children(children.size - 2), idx + 1, Some(idx), node :: lst)
        case children => addSpine(children.last, idx + 1, parentIdx, node :: lst)
      }
      // if (node.label._1.isPrep) node :: lst
      // else node.rightChildren match {
      //   case Seq() => Nil
      //   case children => addSpine(children.last, node :: lst)
      // }
    addSpine(tree, 0, None, Nil) match {
      case (Some(parentIdx), lst) => (lst.reverse.toIndexedSeq, parentIdx)
      case _ => sys.error("Prep not found.")
    }
  }
}
