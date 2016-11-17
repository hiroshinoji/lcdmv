package lcdmv.model

import lcdmv.const.{Direction, Valence}
import lcdmv.data.{PoS, Sentence}

trait ConstAbbreb {
  def Right = Direction.right
  def Left = Direction.left
  def NoChild = Valence.noChild
  def HasChild = Valence.hasChild
}

/** ParamCounter manages how to update chart entries during EM.
  *
  * Different ParamHolder may be used for different assumption (parameterization) of modeling (PCFG).
  */
trait ParamHolder[Param] extends ConstAbbreb {
  val sentence: Sentence
  val token: Int => Int

  val param: Param
  val partial: Param

  def addPartialToCounts(counts: Param): Param
  // def addTwoCounts(lhs: Param, rhs: Param): Param

  def clearPartial(size: Int): Unit

  // def distanceCost(headIdx: Int, depIdx: Int) = distanceCost(headIdx, depIdx)

  val distanceCost: ParamHolder.DistanceCostCalculator
  val compCost: ParamHolder.CompCostCalculator
  val proceedCost: ParamHolder.ProceedCostCalculator

  //val lengthConstraint: ParamHolder.LengthConstraint
  val linkPOSConstraint: ParamHolder.LinkPOSConstraint
  val rootPOSConstraint: ParamHolder.RootPOSConstraint

  val rules: Rules // this is the core part which is implemented in the subclass

  def normalizePartial(marginal: Double): Unit

  trait Rule {
    def score: Double
    def increment(count: Double): Unit // increment partial counts
  }

  /** Set of rules used in left-corner type of split-head PCFG.
    *
    * This set might be growing if new rule is introduced specific for new PCFG.
    */
  trait Rules {
    def selectRoot(head: Int): Rule
    def initiateLeftHalf(head: Int): Rule
    def insertRightPred(inserted: Int): Rule
    def leftPred(leftEdge: Int, head: Int, rightEdge: Int, predHead: Int): Rule
    def rightPred(head: Int, rightEdge: Int, predDep: Int): Rule
    def leftCompFirst(left: Int, right: Int, rightHead: Int, pred: Int): Rule
    def leftCompSecond(left: Int, right: Int, rightEdge: Int, pred: Int): Rule
    def rightComp(head: Int, rightEdge: Int, rightEdge2: Int, pred: Int): Rule
  }
}

object ParamHolder {
  trait DistanceCostCalculator extends ((Int, Int)=>Double) {
    def dist(headIdx: Int, depIdx: Int) = Math.abs(headIdx - depIdx)
    def apply(headIdx: Int, depIdx: Int): Double
  }

  object NoneDistanceCostCalculator extends DistanceCostCalculator {
    def apply(headIdx: Int, depIdx: Int) = 1.0
  }

  class HardDistanceCostCalculator(maxLength: Int) extends DistanceCostCalculator {
    def apply(headIdx: Int, depIdx: Int) =
      if (dist(headIdx, depIdx) <= maxLength) 1.0
      else 0.0
  }

  /** Up to offset length dependency takes no penalty (1, if one wants to add no penalty to length 1 dependency, which is minimum)
    */
  class ExpDistanceCostCalculator(beta: Double, offset: Int = 1) extends DistanceCostCalculator {
    def apply(headIdx: Int, depIdx: Int) = {
      Math.exp(-beta * Math.max(dist(headIdx, depIdx) - offset, 0))
    }
  }

  class LimitedExpDistanceCostCalculator(maxLength: Int, beta: Double, offset: Int = 1) extends DistanceCostCalculator {
    def apply(headIdx: Int, depIdx: Int) = {
      val d = dist(headIdx, depIdx)
      if (d <= maxLength) Math.exp(-beta * Math.max(d - offset, 0))
      else 0.0
    }
  }

  trait CompCostCalculator extends ((Int)=>Double) {
    def apply(chunkSize: Int): Double
  }

  object NoneCompCostCalculator extends CompCostCalculator {
    def apply(chunkSize: Int) = 1.0
  }

  class ExpCompCostCalculator(
    n: Int,
    beta: Double,
    offset: Int = 0) extends CompCostCalculator {
    def apply(chunkSize: Int) = {
      // val x = (-beta / n) * Math.max(chunkSize - offset, 0)
      // Math.exp(x)
      Math.exp(-beta * Math.max(chunkSize - offset, 0))
    }
  }

  trait RootPOSConstraint {
    protected val activeIndicator: IndexedSeq[Boolean]
    def isOK(idx: Int): Boolean = activeIndicator(idx)
  }

  object NoRootConstrait extends RootPOSConstraint {
    val activeIndicator = IndexedSeq()
    override def isOK(idx: Int) = true
  }

  class RootVerbConstraint(s: Sentence) extends RootPOSConstraint {
    val activeIndicator = s.posContains(_.isVerb) match {
      case true => (0 until s.size) map { s.pos(_).isVerb }
      case false => (0 until s.size) map (_=>true)
    }
  }

  /** Basically verb is only candidates but if not exist noun becomes candidates
    */
  class RootVerbOrNounConstraint(s: Sentence) extends RootPOSConstraint {
    val activeIndicator = s.posContains(_.isVerb) match {
      case true => (0 until s.size) map { s.pos(_).isVerb }
      case false => s.posContains(_.isNoun) match {
        case true => (0 until s.size) map { s.pos(_).isNoun }
        case false => (0 until s.size) map (_=>true)
      }
    }
  }

  /** Both verb and noun can be candidates equally
    */
  class RootBothVerbAndNounConstraint(s: Sentence) extends RootPOSConstraint {
    val activeIndicator = s.posContains { p => p.isVerb || p.isNoun } match {
      case true => (0 until s.size) map { i => s.pos(i).isVerb || s.pos(i).isNoun }
      case false => (0 until s.size) map (_=>true)
    }
  }

  class RootVerbNounAdjConstraint(s: Sentence) extends RootPOSConstraint {
    val activeIndicator = s.posContains { p => p.isVerb || p.isNoun || p.isAdj } match {
      case true => (0 until s.size) map { i => s.pos(i).isVerb || s.pos(i).isNoun || s.pos(i).isAdj }
      case false => (0 until s.size) map (_=>true)
    }
  }

  trait LinkPOSConstraint {
    def isOK(head: Int, dep: Int): Boolean
  }

  object NoLinkPOSConstraint extends LinkPOSConstraint {
    def isOK(head: Int, dep: Int) = true
  }

  class LinkPOSConstraintGen(activeLinks: Set[(String, String)]) {

    def existLink(head: PoS, dep: PoS) = activeLinks.contains((head.upos, dep.upos))

    def activeLinkPOSConstraint(s: Sentence) = new LinkPOSConstraint {
      val linkMap: IndexedSeq[IndexedSeq[Boolean]] =
        (0 until s.size).map { h =>
          (0 until s.size).map { d =>
            if (h == d) false else existLink(s.pos(h), s.pos(d))
          }
        }
      val numCands = linkMap.map(_.count(_==true)).sum

      val isOKFunc: (Int, Int)=>Boolean =
        if (numCands == 0) (_, _) => true
        else linkMap(_)(_)

      def isOK(head: Int, dep: Int) = isOKFunc(head, dep)
    }
  }

  object LinkPOSConstraint {

    // val contentPOS = IndexedSeq("ADJ", "INTJ", "NOUN", "NUM", "PRON", "PROPN", "SYM", "VERB", "X")
    val contentPOS = IndexedSeq("ADJ", "ADV", "INTJ", "NOUN", "NUM", "PRON", "PROPN", "SYM", "VERB", "X")

    // val functionalPOS = IndexedSeq("ADP", "ADV", "AUX", "CONJ", "DET", "PART", "SCONJ")
    val functionalPOS = IndexedSeq("ADP", "AUX", "CONJ", "DET", "PART", "SCONJ")

    def functionWordMustBeLeaveConstraint(): (Sentence => LinkPOSConstraint) = {
      val activeLinks = contentPOS.flatMap { head =>
        (contentPOS ++ functionalPOS).map { dep => (head, dep) }
      }.toSet

      val gen = new LinkPOSConstraintGen(activeLinks)

      s => gen.activeLinkPOSConstraint(s)
    }
  }

  trait ProceedCostCalculator {
    def apply(headIdx: Int, dir: Direction, v: Valence): Double
  }

  object NoProceedCostCalculator extends ProceedCostCalculator {
    def apply(headIdx: Int, dir: Direction, v: Valence) = 1.0
  }

  class UnheadProceedCostCalculator(s: Sentence, unheadPOS: Set[String])
      extends ProceedCostCalculator {

    val _headable: Array[Boolean] = (0 until s.size).map { i =>
      !unheadPOS.contains(s.pos(i).upos)
    }.toArray
    val headable =
      if (_headable.forall(!_)) Array.fill(s.size)(true)
      else _headable

    def apply(headIdx: Int, dir: Direction, v: Valence) =
      if (!headable(headIdx)) 0.0
      else 1.0
  }

  class UnheadProceedCostCalcGen(unheadPOS: Set[String]) {
    def mkCalc(s: Sentence) = new UnheadProceedCostCalculator(s, unheadPOS)
  }
}
