package lcdmv.model

import lcdmv.const.{Valence, Direction}

trait HarmonicParamHolder extends DMVParamHolder {
  override def stop(headIdx: Int, dir: Direction, v: Valence) = 1.0
  override def proceed(headIdx: Int, dir: Direction, v: Valence) = 1.0
  override def choice(headIdx: Int, dir: Direction, depIdx: Int) = 1.0 / (Math.abs(headIdx - depIdx))
  override def root(w: Int) = 1.0
}

/** All settings (e.g., sentence, token) are collected in ParamHolder.
  * Users may define their own ParamHolder subclass which defines all settings and mix-in into this trait.
  */
trait DMVParamHolder extends ParamHolder[DMVParameter] {
  def stop(headIdx: Int, dir: Direction, v: Valence) =
    param.gen(token(headIdx), dir, v).stop
  def proceed(headIdx: Int, dir: Direction, v: Valence) =
    param.gen(token(headIdx), dir, v).proceed * proceedCost(headIdx, dir, v)
  def choice(headIdx: Int, dir: Direction, depIdx: Int) =
    if (linkPOSConstraint.isOK(headIdx, depIdx))
      param.choice(token(headIdx), dir)(token(depIdx)) * distanceCost(headIdx, depIdx)
    else 1e-12
  def root(w: Int) =
    if (rootPOSConstraint.isOK(w)) param.root(token(w)) else 1e-12

  def addPartialToCounts(counts: DMVParameter): DMVParameter = {
    counts.addPartial(partial, token, sentence.size)
    counts
  }

  def clearPartial(size: Int) = partial.clear()

  val rules = new OriginalDMVRules {}

  def normalizePartial(marginal: Double) = {
    partial.divideBy(marginal)
    //partial.normalize
  }

  trait OriginalDMVRules extends Rules {
    def selectRoot(head: Int) = new Rule {
      val rightValence = Valence(head == sentence.size - 1)
      val score = root(head) * stop(head, Right, rightValence)
      def increment(count: Double) = {
        partial.incrementRoot(head, count)
        partial.incrementStop(head, Right, rightValence, count)
      }
    }

    def initiateLeftHalf(head: Int) = new Rule {
      val score = stop(head, Left, NoChild)
      def increment(count: Double) = partial.incrementStop(head, Left, NoChild, count)
    }

    def insertRightPred(inserted: Int) = new Rule {
      val score = stop(inserted, Right, NoChild)
      def increment(count: Double) =
        partial.incrementStop(inserted, Right, NoChild, count)
    }

    def leftPred(leftEdge: Int, head: Int, rightEdge: Int, predHead: Int) = new Rule {
      // assert(leftEdge <= headIdx && headIdx <= rightEdge && rightEdge < predHead)
      val headRightValence = Valence(head == rightEdge)
      val predLeftValence = Valence(rightEdge + 1 == predHead)
      override val score = choice(predHead, Left, head) *
      stop(head, Right, headRightValence) *
      proceed(predHead, Left, predLeftValence) *
      stop(predHead, Left, HasChild)

      override def increment(count: Double) = {
        partial.incrementChoice(predHead, Left, head, count)
        partial.incrementStop(head, Right, headRightValence, count)
        partial.incrementProceed(predHead, Left, predLeftValence, count)
        partial.incrementStop(predHead, Left, HasChild, count)
      }
    }

    def rightPred(head: Int, rightEdge: Int, predDep: Int) = new Rule {
      // assert(head <= rightEdge && rightEdge < predDep)
      val predLeftValence = Valence(rightEdge + 1 == predDep)
      val headRightValence = Valence(head == rightEdge)
      override val score = choice(head, Right, predDep) *
      stop(predDep, Left, predLeftValence) *
      proceed(head, Right, headRightValence)

      override def increment(count: Double) = {
        partial.incrementChoice(head, Right, predDep, count)
        partial.incrementStop(predDep, Left, predLeftValence, count)
        partial.incrementProceed(head, Right, headRightValence, count)
      }
    }

    def leftCompFirst(left: Int, right: Int, rightHead: Int, pred: Int) = new Rule {
      // assert(left <= right && right < rightHead && rightHead < pred)
      override def score = choice(pred, Left, rightHead) *
        compCost(rightHead - (right + 1))
      override def increment(count: Double) =
        partial.incrementChoice(pred, Left, rightHead, count)
    }

    def leftCompSecond(
      left: Int, right: Int, rightEdge: Int, pred: Int) = new Rule {
      val hRightValence = Valence(right == rightEdge)
      val pLeftValence = Valence(rightEdge + 1 == pred)
      override def score = stop(right, Right, hRightValence) *
      proceed(pred, Left, pLeftValence) *
      compCost(rightEdge - right)

      override def increment(count: Double) = {
        partial.incrementStop(right, Right, hRightValence, count)
        partial.incrementProceed(pred, Left, pLeftValence, count)
      }
    }

    def rightComp(head: Int, rightEdge: Int, rightEdge2: Int, pred: Int) = new Rule {
      // assert(head <= rightEdge && rightEdge < rightEdge2 && rightEdge2 < pred)
      val mPlusRightValence = Valence(rightEdge + 1 == rightEdge2)
      val pLeftValence = Valence(rightEdge2 + 1 == pred)
      override def score = choice(rightEdge + 1, Right, pred) *
      proceed(rightEdge + 1, Right, mPlusRightValence) *
      stop(rightEdge + 1, Right, HasChild) *
      stop(pred, Left, pLeftValence) *
      compCost(rightEdge2 - (rightEdge + 1))

      override def increment(count: Double) = {
        partial.incrementChoice(rightEdge + 1, Right, pred, count)
        partial.incrementProceed(rightEdge + 1, Right, mPlusRightValence, count)
        partial.incrementStop(rightEdge + 1, Right, HasChild, count)
        partial.incrementStop(pred, Left, pLeftValence, count)
      }
    }
  }
}
