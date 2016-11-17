package lcdmv.model

import lcdmv.data.{PoS, Sentence}

/** This class handles the condition whether a node can be a leaf node (having no child).
  *
  * Basically, only tokens whose POS tag is prohibited to be a leaf node is splitted
  * into two states: already has at least one (left) dependent, or not yet.
  */
trait LeafCondCalculator {

  import LeftCornerSplitHeadNonterminals._

  /** Number of returned values
    */
  val childCondSize: Int

  def rightPredCost(parent: RightFullPred): Double

  /** This has generally a different meaning than rightPredable, but
    * currently its behavior always equals to rightPredable.
    */
  def rightCompCost(parent: RightFullPred) = rightPredCost(parent)

  def leftCompCost(parent: RightFullPred): Double

  def leftCompChunkCost(leftChunkSize: Int, rightFull: RightFull): Double

  def combineCost(left: Int, head: Int, right: Int): Double

  def insertRightPredCost(inserted: Int, predHasChild: Boolean): Double

  def possiblePredChildBeforeComp(pred: Int): Seq[Boolean]
  // head: Int, rightEdge: Int, rightEdge2: Int): Seq[Boolean]

  def hasChildAfterRightPred(pred: Int): Boolean

  def hasChildAfterRightComp(pred: Int) = hasChildAfterRightPred(pred)

  def logicallyOK(node: RightFullPred): Boolean
}

object LeafCondCalculator {

  def posBasedCalcGen(unleafableUpos: Set[String]): Sentence=>LeafCondCalculator =
    new POSBasedLeafCostCalculator(_, p => unleafableUpos.contains(p.upos))

  val adpOnlyConstraintGen: Sentence=>LeafCondCalculator =
    new POSBasedLeafCostCalculator(_, _.upos == "ADP")
}

object NoLeafCondCalculator extends LeafCondCalculator {

  import LeftCornerSplitHeadNonterminals._

  val childCondSize = 1

  def rightPredCost(parent: RightFullPred) = 1.0

  def leftCompCost(parent: RightFullPred) = 1.0

  def leftCompChunkCost(leftChunkSize: Int, rightFull: RightFull) = 1.0

  def combineCost(left: Int, head: Int, right: Int) = 1.0

  def insertRightPredCost(inserted: Int, predHasChild: Boolean) = 1.0

  def possiblePredChildBeforeComp(pred: Int) = Seq(true)

  def hasChildAfterRightPred(pred: Int) = true

  def logicallyOK(node: RightFullPred) = {
    assert(node.predHasChild)
    true
  }
}

/** If one wants to prefer functional head, by prohibiting each ADP to become a leaf node, add ADP to unleafable; then, each ADP must have at least one child.
  */
class POSBasedLeafCostCalculator(
  val sentence: Sentence,
  val unleafable: PoS => Boolean) extends LeafCondCalculator {

  private val leafableSeq: IndexedSeq[Boolean] =
    if (sentence.size == 1) IndexedSeq(true)
    else (0 until sentence.size).map(i => !unleafable(sentence.pos(i)))

  import LeftCornerSplitHeadNonterminals._

  val childCondSize = 2

  /** RightFullPred just after the RightPred (or RightComp) must not have a child.
    */
  def rightPredCost(parent: RightFullPred) =
    if (allowedPOS(parent.pred) || !parent.predHasChild) 1.0
    else 0.0

  /** After LeftComp, the pred of parent must have a child
    */
  def leftCompCost(parent: RightFullPred) =
    if (allowedPOS(parent.pred) || parent.predHasChild) 1.0
    else 0.0

  def leftCompChunkCost(leftChunkSize: Int, rightFull: RightFull) =
    if (allowedPOS(rightFull.head) || leftChunkSize + rightFull.m >= 1) 1.0
    else 0.0

  def possiblePredChildBeforeComp(pred: Int) =
    if (allowedPOS(pred)) Seq(true)
    else Seq(true, false)

  def combineCost(left: Int, head: Int, right: Int) =
    if (allowedPOS(head) || right - left > 0) 1.0
    else 0.0

  def insertRightPredCost(inserted: Int, predHasChild: Boolean) =
    if (predHasChild || allowedPOS(inserted)) 1.0
    else 0.0

  /** noCostPOS means it can be a leaf node at anytime.
    * This design is due to the fact that # of unleafable POS would be smaller than
    * # of leafable POS.
    */
  def allowedPOS(idx: Int) = leafableSeq(idx)

  /** pred does not have a child after rightPred (except it is allowed word)
    */
  def hasChildAfterRightPred(pred: Int) = allowedPOS(pred)

  def logicallyOK(node: RightFullPred) = {
    if (allowedPOS(node.pred)) node.predHasChild // only true is possible
    else {
      if (node.head == node.edge)
        !node.predHasChild // for splittable node, hasChild value must be false when span length = 1
      else true
    }
  }
}
