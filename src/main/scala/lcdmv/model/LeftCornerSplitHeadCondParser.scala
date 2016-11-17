package lcdmv.model

import lcdmv.const.{Direction, Valence}
import lcdmv.data.{DepArcs, NormalDepArcs, PoS, Sentence}

trait LeftCornerSplitHeadCondParser[Param] extends Parser[Param] {

  import nonterms._
  type Nonterm = CondNonterm
  type N = Nonterminal

  trait CondTable[I] extends ChartTable[CondNonterm, I] {

    private val d = maxDepth match { case 0 => 1; case d => d }
    private val m = maxChunkSize match { case 0 => 1; case l => l + 1 }
    lazy val childCondSize = mkLeafCondition(Sentence.empty).childCondSize

    /** Note that d = 0 means the first element of the stack is used, i.e., the actual depth is 1. Also, m = 0 means the actual span length = 1 (one word span). If maxChunkSize = 1, m = 1 abstracts all other cases (m >= 1).
      */
    private val s = threeDim()
    private val triangle = threeDimWithDepth()
    private val leftFull = twoDimWithDepth()
    private val rightFull = twoDimWithDepth()
    private val squareFullPred = threeDimWithDepth()
    private val squareFragPred = threeDimWithDepthChunk()
    // private val rightFullPred = threeDimWithDepth()
    private val rightFullPred = threeDimWithDepthChildCond()
    private val rightFragPred = threeDimWithDepthChunk()

    private val v: Array[N=>Accessor] = {
      val offset1 = size * size * 2
      val offset2 = size * 2

      val v1:Array[Int] = (0 until size).map(_ * offset1).toArray
      val v2:Array[Int] = (0 until size).map(_ * offset2).toArray

      Array(
        x => Accessor(s, v1(x.i) + v2(x.j) + x.k + x.k),
        x => Accessor(triangle(x.d), v1(x.i) + v2(x.j) + x.k + x.k),
        x => Accessor(leftFull(x.d), v2(x.i) + x.j + x.j),
        x => Accessor(rightFull(x.d), v2(x.i) + x.j + x.j),
        x => Accessor(squareFullPred(x.d), v1(x.i) + v2(x.j) + x.k + x.k),
        x => Accessor(squareFragPred(x.d)(x.m), v1(x.i) + v2(x.j) + x.k + x.k),
        x => Accessor(rightFullPred(x.d)(x.p), v1(x.i) + v2(x.j) + x.k + x.k),
        x => Accessor(rightFragPred(x.d)(x.m), v1(x.i) + v2(x.j) + x.k + x.k)
      )
    }

    /** Only clear counts that would be used for next sentence of length n
      */
    def clearCounts(n: Int) = {
      def clearArray(data: Array[Option[I]]) =
        for (i <- 0 until data.size) data(i) = None

      clearArray(s)
      for (depth <- triangle) clearArray(depth)
      for (depth <- leftFull) clearArray(depth)
      for (depth <- rightFull) clearArray(depth)
      for (depth <- squareFullPred) clearArray(depth)
      for (depth <- squareFragPred; m <- depth) clearArray(m)
      for (depth <- rightFullPred; p <- depth) clearArray(p)
      for (depth <- rightFragPred; m <- depth) clearArray(m)
    }

    def twoDimWithDepth() =
      Array.fill[Option[I]](d, size * size * 2)(None)

    def threeDimWithDepth() =
      Array.fill[Option[I]](d, size * size * size * 2)(None)

    def threeDimWithDepthChunk() =
      Array.fill[Option[I]](d, m, size * size * size * 2)(None)

    def threeDimWithDepthChildCond() =
      Array.fill[Option[I]](d, childCondSize, size * size * size * 2)(None)

    def threeDimWithDepthChunkCond() =
      Array.fill(d)(Array.fill[Option[I]](m, childCondSize, size * size * size * 2)(None))

    private def threeDim() =
      Array.fill[Option[I]](size * size * size * 2)(None)

    def getHelper(condNonterm: CondNonterm): Accessor = { // (Array[Option[I]], Int) = {
      val nonterm = condNonterm.nonterm
      val cond = if (condNonterm.cond) 1 else 0

      val ret = v(nonterm.id)(nonterm)
      ret.k += cond
      ret
    }

    val isOK: (CondNonterm=>Boolean) =
      if (maxDepth == 0) _ => true // _.nonterm.d >= 0
      else x => x.nonterm.d < d && x.nonterm.d >= 0
  }
}

trait LeftCornerSplitHeadExpectingCondParser[Param]
    extends LeftCornerSplitHeadCondParser[Param] with ExpectingParser[Param] {

  import nonterms._

  type Item = Double
  type Table = CondTable[Double]

  def mkTable(n: Int) = new CondTable[Double] {
    def size = n
    def blankItem = 0
  }

  def mkChart(sentence: Sentence, partial: Param, inside: Table, outside: Table) =
    new CondExpectingChart(sentence, partial, inside, outside)

  class CondExpectingChart(
    val sentence: Sentence,
    val partial: Param,
    val insideTable: Table,
    val outsideTable: Table) extends Chart  {

    def marginal() = (0 to n - 1).map { r =>
      insideScore(CondNonterm(S(0, r, n - 1), true))
    }.sum

    def insideScore(nonterm: CondNonterm): Double = insideTable.getOrUpdate(nonterm) {
      case CondNonterm(S(b, h, e), cond) =>
        if (!cond) 0.0 // cond must be satisfied
        else {
          assert(b == 0 && e == n - 1)
          unaryInsideWithCheck(
            cond, // = true
            Triangle(b, h, e, 0),
            rules.selectRoot(h).score)
        }
      case CondNonterm(Triangle(i, j, k, d), cond) =>
        binaryInsideWithCheck(
          cond,
          LeftFull(i, j, d),
          RightFull(j, k, d),
          leafCond.combineCost(i, j, k))
      case CondNonterm(LeftFull(i, j, d), cond) if i == j =>
        if (n != 1 && cond) 0.0 // leaf must not be satisfied. exception is one word sentence
        else rules.initiateLeftHalf(i).score
      case CondNonterm(LeftFull(i, j, d), cond) =>
        unaryInsideWithCheck(
          cond,
          SquareFullPred(i, j - 1, j, d),
          1.0)
      case CondNonterm(RightFull(i, j, d), cond) if i == j =>
        if (cond) 0.0
        else 1.0
      case CondNonterm(RightFull(i, j, d), cond) =>
        unaryInsideWithCheck(
          cond,
          RightFullPred(i, j - 1, j, d, true), // WARNING: is it safe?
          rules.insertRightPred(j).score)
      case CondNonterm(SquareFullPred(b, e, p, d), cond) =>
        var sum = 0.0
        // leftPredScore
        (b to e) foreach { h =>
          sum += unaryInsideWithCheck(
            cond,
            Triangle(b, h, e, d),
            rules.leftPred(b, h, e, p).score,
            (p, h))
        }
        // leftCompScore
        (b + 1 to e) foreach { h =>
          depthCalc.possibleLeftChunkSizes(b, h) foreach { m =>

            val rightFull = rightFullForComp(h, e, d, m)
            sum += binaryInsideWithCheck(
              cond,
              SquareFragPred(b, h, p, d, m),
              rightFull,
              rules.leftCompSecond(b, h, e, p).score *
                leafCond.leftCompChunkCost(m, rightFull))
          }
        }
        sum
      case CondNonterm(SquareFragPred(b, e, p, d, m), cond) =>
        var sum = 0.0
        depthCalc.possibleLeftCompSegments(b, e, m) foreach { segment =>
          sum += binaryInsideWithCheck(
            cond,
            SquareFullPred(b, segment, p, d),
            LeftFull(segment + 1, e, d),
            rules.leftCompFirst(b, segment, e, p).score,
            (p, e))
        }
        sum
      case CondNonterm(parent: RightFullPred, cond)
          if !leafCond.logicallyOK(parent) => 0.0
      case CondNonterm(parent @ RightFullPred(h, e, p, d, predHasChild), cond) =>
        var sum = 0.0
        // rightPred

        val rightPredCost = leafCond.rightPredCost(parent)
        val rightCompCost = leafCond.rightCompCost(parent)
        val leftCompCost = leafCond.leftCompCost(parent)

        if (rightPredCost > 0.0) sum += unaryInsideWithCheck(
          cond,
          RightFull(h, e, d),
          rules.rightPred(h, e, p).score * rightPredCost,
          (h, p))

        // leftComp
        if (leftCompCost > 0.0) for (segment <- h + 1 to e;
          m <- depthCalc.possibleLeftChunkSizes(h, segment)) {

          val rightFull = rightFullForComp(segment, e, d, m)

          sum += binaryInsideWithCheck(
            cond,
            RightFragPred(h, segment, p, d, m),
            rightFull,
            rules.leftCompSecond(h, segment, e, p).score *
              leftCompCost *
              leafCond.leftCompChunkCost(m, rightFull))
        }

        // rightComp
        if (rightCompCost > 0.0) for (segment <- h to e - 1;
          score = rules.rightComp(h, segment, e, p).score * rightCompCost;
          if score > 0.0;
          predHasChild <- leafCond.possiblePredChildBeforeComp(segment + 1);
          rightFullPred = RightFullPred(h, segment, segment + 1, d, predHasChild)) {
          sum += binaryInsideWithCheck(
            cond,
            rightFullPred,
            rightFullForComp(segment + 1, e, d, 0),
            score,
            (segment + 1, p))
        }
        // if (!predHasChild) { // これで check してはダメ

        //   (h to e - 1) foreach { segment =>
        //     val leftsPredChild =
        //       if (unleafable(segment + 1) || e - (segment + 1) > 0) Seq(true, false)
        //       else Seq(true)
        //     for (pc <- leftsPredChild) {
        //       RightFullPred(h, segment, segment + 1, d, pc) を使って計算
        //     }
        //   sum += binaryInsideWithCheck(
        //     cond,
        //     RightFullPred(h, segment, segment + 1, d),
        //     rightFullForComp(segment + 1, e, d, 0),
        //     rules.rightComp(h, segment, e, p).score,
        //     (segment + 1, p))
        // }
        sum
      case CondNonterm(RightFragPred(h, e, p, d, m), cond) =>
        var sum = 0.0
        for (segment <- depthCalc.possibleLeftCompSegments(h, e, m);
          leftFull = LeftFull(segment + 1, e, d);
          predHasChild <- leafCond.possiblePredChildBeforeComp(p)) {
          sum += binaryInsideWithCheck(
            cond,
            RightFullPred(h, segment, p, d, predHasChild),
            leftFull,
            rules.leftCompFirst(h, segment, e, p).score,
            (p, e))
        }
        sum
    }

    private def unaryInsideWithCheck(
      cond: Boolean, child: N, score: Double): Double =
      if (score == 0.0) 0.0
      else insideScore(CondNonterm(child, cond)) * score

    private def binaryInsideWithCheck(
      cond: Boolean, left: N, right: N, score: Double): Double =
      if (score == 0.0) 0.0
      else {
        val leftFalse = insideScore(CondNonterm(left, false))
        val rightFalse = insideScore(CondNonterm(right, false))

        if (cond) {
          val leftTrue = insideScore(CondNonterm(left, true))
          val rightTrue = insideScore(CondNonterm(right, true))
          (leftTrue * rightTrue + leftTrue * rightFalse + leftFalse * rightTrue) * score
        } else {
          leftFalse * rightFalse * score
        }
      }

    private def unaryInsideWithCheck(
      cond: Boolean, child: N, score: Double, link: (Int, Int)): Double =
      if (score == 0.0) 0.0
      else {
        val satisfy = condition.satisfy(link._1, link._2)

        if (cond) {
          val alreadySatisfied = insideScore(CondNonterm(child, true)) * score
          val justSatisfied =
            if (satisfy) insideScore(CondNonterm(child, false)) * score
            else 0.0
          alreadySatisfied + justSatisfied
        } else {
          if (satisfy) 0.0 // contradict (child satisfies condition but parent does not)
          else insideScore(CondNonterm(child, false)) * score
        }
      }

    private def binaryInsideWithCheck(
      cond: Boolean, left: N, right: N, score: Double, link: (Int, Int)): Double =
      if (score == 0.0) 0.0
      else {
        val satisfy = condition.satisfy(link._1, link._2)

        val leftFalse = insideScore(CondNonterm(left, false))
        val rightFalse = insideScore(CondNonterm(right, false))

        if (cond) {
          val leftTrue = insideScore(CondNonterm(left, true))
          val rightTrue = insideScore(CondNonterm(right, true))

          val alreadySatisfied =
            (leftTrue * rightTrue + leftTrue * rightFalse + leftFalse * rightTrue) * score

          val justSatisfied =
            if (satisfy) leftFalse * rightFalse * score
            else 0.0
          alreadySatisfied + justSatisfied
        } else {
          if (satisfy) 0.0 // coontradict!
          else leftFalse * rightFalse * score
        }
      }

    def outsideScore(nonterm: CondNonterm): Double = outsideTable.getOrUpdate(nonterm) {
      case CondNonterm(S(b, h, e), cond) =>
        assert(b == 0 && e == n - 1)
        if (cond) 1.0 else 0.0
      case CondNonterm(Triangle(b, h, e, d), cond) =>
        var sum = 0.0

        // leftPred
        (e + 1 to n - 1) foreach { p =>
          sum += unaryOutsideWithCheck(
            cond,
            SquareFullPred(b, e, p, d),
            rules.leftPred(b, h, e, p).score,
            (p, h))
        }
        val rootScore =
          if (b != 0 || e != n - 1 || d != 0) 0.0
          else unaryOutsideWithCheck(cond, S(b, h, e), rules.selectRoot(h).score)
        sum + rootScore
      case CondNonterm(LeftFull(e, h, d), cond) =>
        var sum = 0.0
        // combine
        (h to n - 1) foreach { rightEdge =>
          sum += binaryOutsideWithCheck(
            cond,
            Triangle(e, h, rightEdge, d),
            RightFull(h, rightEdge, d),
            leafCond.combineCost(e, h, rightEdge))
        }
        // leftComp
        for (leftBegin <- 0 to e - 1;
          // chunkSize = depthCalc.leftChunkSize(h - e);
          chunkSize = depthCalc.leftChunkSize(e, h);
          p <- h + 1 to n - 1) {

          val localScore = rules.leftCompFirst(leftBegin, e - 1, h, p).score
          sum += binaryOutsideWithCheck(
            cond,
            SquareFragPred(leftBegin, h, p, d, chunkSize),
            SquareFullPred(leftBegin, e - 1, p, d),
            localScore,
            (p, h))

          for (predHasChild <- leafCond.possiblePredChildBeforeComp(p)) {
            sum += binaryOutsideWithCheck(
              cond,
              RightFragPred(leftBegin, h, p, d, chunkSize),
              RightFullPred(leftBegin, e - 1, p, d, predHasChild),
              localScore,
              (p, h))
          }
        }
        sum
      case CondNonterm(rightFull @ RightFull(h, e, d), cond) =>
        var sum = 0.0
        // combine
        for (leftEdge <- 0 to h) {
          sum += binaryOutsideWithCheck(
            cond,
            Triangle(leftEdge, h, e, d),
            LeftFull(leftEdge, h, d),
            leafCond.combineCost(leftEdge, h, e))
        }

        // rightPred
        for (p <- e + 1 to n - 1) {
          sum += unaryOutsideWithCheck(
            cond,
            RightFullPred(h, e, p, d, leafCond.hasChildAfterRightPred(p)),
            rules.rightPred(h, e, p).score,
            (h, p))
        }

        // leftComp
        for (leftBegin <- 0 to h - 1;
          m <- depthCalc.possibleLeftChunkSizes(leftBegin, h);
          p <- e + 1 to n - 1) {

          val localScore = rules.leftCompSecond(leftBegin, h, e, p).score *
          leafCond.leftCompChunkCost(m, rightFull)

          // val leftHandDepth = d - depthCalc.compDepthDiff(m, e - h)
          val leftHandDepth = d - depthCalc.compDepthDiff(m, h, e)

          sum += binaryOutsideWithCheck(
            cond,
            SquareFullPred(leftBegin, e, p, leftHandDepth),
            SquareFragPred(leftBegin, h, p, leftHandDepth, m),
            localScore)

          sum += binaryOutsideWithCheck(
            cond,
            RightFullPred(leftBegin, e, p, leftHandDepth, true), // this must be true?
            RightFragPred(leftBegin, h, p, leftHandDepth, m),
            localScore)
        }

        // rightComp
        // val leftHandDepth = d - depthCalc.compDepthDiff(e - h)
        val leftHandDepth = d - depthCalc.rightCompDepthDiff(h, e)
        for (leftHead <- 0 to h - 1;
          p <- e + 1 to n - 1;
          parentPredHasChild = leafCond.hasChildAfterRightPred(p);
          childPredHasChild <- leafCond.possiblePredChildBeforeComp(h)) {

          sum += binaryOutsideWithCheck(
            cond,
            RightFullPred(leftHead, e, p, leftHandDepth, parentPredHasChild),
            RightFullPred(leftHead, h - 1, h, leftHandDepth, parentPredHasChild),
            rules.rightComp(leftHead, h - 1, e, p).score,
            (h, p))
        }
        sum
      case CondNonterm(SquareFullPred(b, e, p, d), cond) =>
        var sum = 0.0
        if (p == e + 1) sum += unaryOutsideWithCheck(
          cond,
          LeftFull(b, e + 1, d),
          1.0) // insert

        // leftComp (first)
        (e + 1 to p - 1) foreach { rightHead =>
          sum += binaryOutsideWithCheck(
            cond,
            // SquareFragPred(b, rightHead, p, d, depthCalc.leftChunkSize(rightHead - (e + 1))),
            SquareFragPred(b, rightHead, p, d, depthCalc.leftChunkSize(e + 1, rightHead)),
            LeftFull(e + 1, rightHead, d),
            rules.leftCompFirst(b, e, rightHead, p).score,
            (p, rightHead))
        }
        sum
      case CondNonterm(SquareFragPred(b, e, p, d, m), cond) =>
        var sum = 0.0
        (e to p - 1) foreach { rightEdge =>
          val rightFull = rightFullForComp(e, rightEdge, d, m)
          sum += binaryOutsideWithCheck(
            cond,
            SquareFullPred(b, rightEdge, p, d),
            rightFull,
            rules.leftCompSecond(b, e, rightEdge, p).score *
              leafCond.leftCompChunkCost(m, rightFull))
        }
        sum
      case CondNonterm(rightFullPred: RightFullPred, cond)
          if !leafCond.logicallyOK(rightFullPred) => 0.0

      case CondNonterm(RightFullPred(h, e, p, d, predHasChild), cond) =>
        var sum = 0.0
        // insert
        if (p == e + 1) {
          val insertCost = leafCond.insertRightPredCost(e + 1, predHasChild)
          sum += unaryOutsideWithCheck(
            cond,
            RightFull(h, e + 1, d),
            rules.insertRightPred(e + 1).score * insertCost)
        }

        // leftComp (does not care predHasChild)
        (e + 1 to p - 1) foreach { rightHead =>
          sum += binaryOutsideWithCheck(
            cond,
            // RightFragPred(h, rightHead, p, d, depthCalc.leftChunkSize(rightHead - (e + 1))),
            RightFragPred(h, rightHead, p, d, depthCalc.leftChunkSize(e + 1, rightHead)),
            LeftFull(e + 1, rightHead, d),
            rules.leftCompFirst(h, e, rightHead, p).score,
            (p, rightHead))
        }

        // rightComp (does not care predHasChild)
        if (p == e + 1) (e + 1 to n - 2) foreach { rightEdge =>
          // RightFull.forComp(e + 1, rightEdge, d)
          (rightEdge + 1 to n - 1) foreach { newPred =>
            sum += binaryOutsideWithCheck(
              cond,
              RightFullPred(h, rightEdge, newPred, d, leafCond.hasChildAfterRightComp(newPred)),
              rightFullForComp(e + 1, rightEdge, d, 0),
              rules.rightComp(h, e, rightEdge, newPred).score,
              (e + 1, newPred))
          }
        }
        sum
      case CondNonterm(RightFragPred(h, e, p, d, m), cond) =>
        var sum = 0.0
        (e to p - 1) foreach { rightEdge =>
          val rightFull = rightFullForComp(e, rightEdge, d, m)
          sum += binaryOutsideWithCheck(
            cond,
            RightFullPred(h, rightEdge, p, d, true),
            rightFull,
            rules.leftCompSecond(h, e, rightEdge, p).score *
              leafCond.leftCompChunkCost(m, rightFull))
        }
        sum
    }

    def unaryOutsideWithCheck(cond: Boolean, parent: N, score: Double): Double = {
      outsideScore(CondNonterm(parent, cond)) * score
    }

    def binaryOutsideWithCheck(
      cond: Boolean, parent: N, sibling: N, score: Double): Double = {
      val childTrue = insideScore(CondNonterm(sibling, true))
      val childFalse = insideScore(CondNonterm(sibling, false))

      val parentTrue = outsideScore(CondNonterm(parent, true))
      val parentFalse = outsideScore(CondNonterm(parent, false))

      if (cond) {
        parentTrue * (childTrue + childFalse) * score
      } else {
        (parentTrue * childTrue + parentFalse * childFalse) * score
      }
    }

    def unaryOutsideWithCheck(
      cond: Boolean, parent: N, score: Double, link: (Int, Int)): Double = {
      val satisfy = cond || condition.satisfy(link._1, link._2)
      outsideScore(CondNonterm(parent, satisfy)) * score
    }

    def binaryOutsideWithCheck(
      cond: Boolean, parent: N, sibling: N, score: Double, link: (Int, Int)): Double = {
      val satisfy = condition.satisfy(link._1, link._2)

      val childTrue = insideScore(CondNonterm(sibling, true))
      val childFalse = insideScore(CondNonterm(sibling, false))

      val parentTrue = outsideScore(CondNonterm(parent, true))
      val parentFalse = outsideScore(CondNonterm(parent, false))

      if (cond || satisfy) {
        parentTrue * (childTrue + childFalse) * score
      } else {
        (parentTrue * childTrue + parentFalse * childFalse) * score
      }
    }

    def terminalOutside(idx: Int): Double = {
      val leftInsertScore = candDepth().flatMap { d =>
        (0 to idx - 1).map { s =>
          binaryOutsideWithCheck(
            false,
            LeftFull(s, idx, d),
            SquareFullPred(s, idx - 1, idx, d),
            1.0)
        }
      }.sum
      val initialRightInsertScore = candDepth().flatMap { d =>
        (0 to idx - 1).map { h =>
          binaryOutsideWithCheck(
            false,
            RightFull(h, idx, d),
            RightFullPred(h, idx - 1, idx, d),
            rules.insertRightPred(idx).score)
        }
      }.sum
      val rightCompScore = candDepth().flatMap { d =>
        (0 to idx - 1).flatMap { h =>
          (idx to n - 2).flatMap { e =>
            (e + 1 to n - 1).map { p =>
              binaryInsideWithCheck(
                true,
                RightFullPred(h, idx - 1, idx, d),
                rightFullForComp(idx, e, d, 0),
                rules.rightComp(h, idx - 1, e, p).score *
                  outsideScore(CondNonterm(RightFullPred(h, e, p, d), true)),
                (idx, p))
            }
          }
        }
      }.sum
      val leftFullScore = candDepth().map { d =>
        unaryOutsideWithCheck(
          false,
          LeftFull(idx, idx, d),
          rules.initiateLeftHalf(idx).score)
      }.sum
      val leftHalfScore = leftFullScore + leftInsertScore + initialRightInsertScore + rightCompScore

      val secondRightInsertScore = candDepth().flatMap { d =>
        (0 to idx - 1).map { h =>
          binaryOutsideWithCheck(
            true,
            RightFull(h, idx, d),
            RightFullPred(h, idx - 1, idx, d),
            rules.insertRightPred(idx).score)
        }
      }.sum
      val rightFullScore = candDepth().map { d =>
        unaryOutsideWithCheck(
          true,
          RightFull(idx, idx, d),
          1.0)
      }.sum

      val rightHalfScore = secondRightInsertScore + rightFullScore

      leftHalfScore + rightHalfScore
    }

    private def candDepth() = maxDepth match {
      case 0 => Seq(0)
      case _ => 0 until maxDepth
    }

    def incrementCounts() = {
      // root select
      (0 to n - 1) foreach { rootIdx =>
        val rule = rules.selectRoot(rootIdx)
        rule.increment(
          insideScore(CondNonterm(Triangle(0, rootIdx, n - 1, 0), true)) *
            outsideScore(CondNonterm(S(0, rootIdx, n - 1), true)) *
            rule.score)
      }

      candDepth() foreach { d =>
        // initial left half
        for (idx <- 0 to n - 1) {
          val rule = rules.initiateLeftHalf(idx)
          incrementLeafWithCheck(rule, LeftFull(idx, idx, d))
        }
        // right insert
        for (
          headIdx <- 0 to n - 2;
          edgeIdx <- headIdx to n - 2) {

          val insertedIdx = edgeIdx + 1
          val rule = rules.insertRightPred(insertedIdx) // NOTE: this is ok
          incrementUnaryWithCheck(
            rule,
            RightFullPred(headIdx, edgeIdx, insertedIdx, d, true),
            RightFull(headIdx, edgeIdx + 1, d))
        }
        // left pred
        for (
          begin <- 0 to n - 2;
          end <- begin to n - 2;
          headIdx <- begin to end;
          predIdx <- end + 1 to n - 1) {

          val rule = rules.leftPred(begin, headIdx, end, predIdx)
          incrementUnaryWithCheck(
            rule,
            Triangle(begin, headIdx, end, d),
            SquareFullPred(begin, end, predIdx, d),
            (predIdx, headIdx))
        }
        // right pred
        for (
          headIdx <- 0 to n - 2;
          edgeIdx <- headIdx to n - 2;
          predIdx <- edgeIdx + 1 to n - 1) {

          val rule = rules.rightPred(headIdx, edgeIdx, predIdx)
          incrementUnaryWithCheck(
            rule,
            RightFull(headIdx, edgeIdx, d),
            RightFullPred(headIdx, edgeIdx, predIdx, d,
              leafCond.hasChildAfterRightPred(predIdx)),
            (headIdx, predIdx))
        }
        // left comp (1st part)
        for (
          begin <- 0 to n - 2;
          newEnd <- begin + 1 to n - 2;
          predIdx <- newEnd + 1 to n - 1;
          segment <- begin to newEnd - 1) {

          // val m = depthCalc.leftChunkSize(newEnd - (segment + 1))
          val m = depthCalc.leftChunkSize(segment + 1, newEnd)

          val rule = rules.leftCompFirst(begin, segment, newEnd, predIdx)

          val leftFull = LeftFull(segment + 1, newEnd, d)

          incrementBinaryWithCheck(
            rule,
            SquareFullPred(begin, segment, predIdx, d),
            leftFull,
            SquareFragPred(begin, newEnd, predIdx, d, m),
            (predIdx, newEnd))

          for (hasChild <- leafCond.possiblePredChildBeforeComp(predIdx)) {
            incrementBinaryWithCheck(
              rule,
              RightFullPred(begin, segment, predIdx, d, hasChild),
              leftFull,
              RightFragPred(begin, newEnd, predIdx, d, m),
              (predIdx, newEnd))
          }
        }
        // left comp (2nd part)
        for (
          begin <- 0 to n - 2;
          newEnd <- begin + 1 to n - 2;
          predIdx <- newEnd + 1 to n - 1;
          segment <- begin + 1 to newEnd) {

          val rule = rules.leftCompSecond(begin, segment, newEnd, predIdx)

          for (
            m <- depthCalc.possibleLeftChunkSizes(begin, segment);
            rightFull = rightFullForComp(segment, newEnd, d, m);
            leftCompChunkCost = leafCond.leftCompChunkCost(m, rightFull);
            if leftCompChunkCost > 0) {

            incrementBinaryWithCheck(
              rule,
              rightFull,
              SquareFragPred(begin, segment, predIdx, d, m),
              SquareFullPred(begin, newEnd, predIdx, d))

            incrementBinaryWithCheck(
              rule,
              rightFull,
              RightFragPred(begin, segment, predIdx, d, m),
              RightFullPred(begin, newEnd, predIdx, d, true))
          }
        }
        // right comp
        for (
          begin <- 0 to n - 3;
          end <- begin + 1 to n - 2;
          segment <- begin to end - 1;
          predIdx <- end + 1 to n - 1) {

          val rule = rules.rightComp(begin, segment, end, predIdx)

          for (predOfChildHasChild <- leafCond.possiblePredChildBeforeComp(segment + 1)) {
            incrementBinaryWithCheck(
              rule,
              RightFullPred(begin, segment, segment + 1, d, predOfChildHasChild),
              rightFullForComp(segment + 1, end, d, 0),
              RightFullPred(begin, end, predIdx, d, leafCond.hasChildAfterRightComp(predIdx)),
              (segment + 1, predIdx))
          }
        }
      }
      paramHolder.normalizePartial(marginal)
    }

    type Rule = ParamHolder[Param]#Rule

    private def incrementLeafWithCheck(rule: Rule, node: N) = {
      val outside =
        if (n == 1) outsideScore(CondNonterm(node, true))
        else outsideScore(CondNonterm(node, false))

      if (outside > 0) rule.increment(outside * rule.score)
    }

    private def incrementUnaryWithCheck(rule: Rule, child: N, parent: N) = {
      val outsideTrue = outsideScore(CondNonterm(parent, true))
      val outsideFalse = outsideScore(CondNonterm(parent, false))
      if (outsideTrue > 0 || outsideFalse > 0) {
        val insideTrue = insideScore(CondNonterm(child, true))
        val insideFalse = insideScore(CondNonterm(child, false))
        rule.increment((outsideTrue * insideTrue + outsideFalse * insideFalse) * rule.score)
      }
    }

    private def incrementUnaryWithCheck(
      rule: Rule, child: N, parent: N, link: (Int, Int)) = {
      val satisfy = condition.satisfy(link._1, link._2)
      val outsideTrue = outsideScore(CondNonterm(parent, true))
      val outsideFalse = outsideScore(CondNonterm(parent, false))
      if (outsideTrue > 0 || outsideFalse > 0) {
        val insideTrue = insideScore(CondNonterm(child, true))
        val insideFalse = insideScore(CondNonterm(child, false))

        val score =
          if (satisfy) outsideTrue * (insideTrue + insideFalse) * rule.score
          else (outsideTrue * insideTrue + outsideFalse * insideFalse) * rule.score

        rule.increment(score)
      }
    }

    private def incrementBinaryWithCheck(rule: Rule, left: N, right: N, parent: N) = {
      val outsideTrue = outsideScore(CondNonterm(parent, true))
      val outsideFalse = outsideScore(CondNonterm(parent, false))
      if (outsideTrue > 0 || outsideFalse > 0) {
        val leftTrue = insideScore(CondNonterm(left, true))
        val leftFalse = insideScore(CondNonterm(left, false))
        val rightTrue = insideScore(CondNonterm(right, true))
        val rightFalse = insideScore(CondNonterm(right, false))

        val insideTrue = leftTrue * rightTrue +
        leftTrue * rightFalse +
        leftFalse * rightTrue

        val insideFalse = leftFalse * rightFalse

        rule.increment((outsideTrue * insideTrue + outsideFalse * insideFalse) * rule.score)
      }
    }

    private def incrementBinaryWithCheck(
      rule: Rule, left: N, right: N, parent: N, link: (Int, Int)) = {
      val satisfy = condition.satisfy(link._1, link._2)
      val outsideTrue = outsideScore(CondNonterm(parent, true))
      val outsideFalse = outsideScore(CondNonterm(parent, false))

      if (outsideTrue > 0 || outsideFalse > 0) {
        val leftTrue = insideScore(CondNonterm(left, true))
        val leftFalse = insideScore(CondNonterm(left, false))
        val rightTrue = insideScore(CondNonterm(right, true))
        val rightFalse = insideScore(CondNonterm(right, false))

        val insideTrue = leftTrue * rightTrue +
        leftTrue * rightFalse +
        leftFalse * rightTrue

        val insideFalse = leftFalse * rightFalse

        val score =
          if (satisfy) outsideTrue * (insideTrue + insideFalse) * rule.score
          else (outsideTrue * insideTrue + outsideFalse * insideFalse) * rule.score

        rule.increment(score)
      }
    }
  }
}
