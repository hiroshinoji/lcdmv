package lcdmv.model

import lcdmv.const.{Direction, Valence}
import lcdmv.data.{DepArcs, NormalDepArcs, Sentence}

trait LeftCornerSplitHeadBasicParser[Param] extends Parser[Param] {

  import nonterms._
  type Nonterm = Nonterminal

  trait BasicTable[I] extends ChartTable[Nonterm, I] {
    def size: Int

    private val d = maxDepth match { case 0 => 1; case d => d }
    private val m = maxChunkSize match { case 0 => 1; case l => l + 1 }

    /** Note that d = 0 means the first element of the stack is used, i.e., the actual depth is 1. Also, m = 0 means the actual span length = 1 (one word span). If maxChunkSize = 1, m = 1 abstracts all other cases (m >= 1).
      */
    private val v = Array(
      threeDim(), // S
      threeDimWithDepth(), // Triangle
      twoDimWithDepth(), // LeftFull
      twoDimWithDepth(), // RightFull
      threeDimWithDepth(), // SquareFullPred
      threeDimWithDepthChunk(), // SquareFragPred
      threeDimWithDepth(), // RightFullPred
      threeDimWithDepthChunk() // RightFragPred
    )

    def clearCounts(n: Int) = for (
      nonterm <- v;
      depth <- nonterm;
      m <- depth;
      i <- 0 until n) {

      val ith = m(i)
      for (j <- 0 until n) {
        val jth = ith(j)
        val maxk = if (jth.size == 1) 1 else n
        for (k <- 0 until maxk) {
          jth(k) = None
        }
      }
    }

    def twoDimWithDepth() = Array.fill[Option[I]](d, 1, size, size, 1)(None)
    def threeDimWithDepth() = Array.fill[Option[I]](d, 1, size, size, size)(None)
    def threeDimWithDepthChunk() = Array.fill[Option[I]](d, m, size, size, size)(None)
    private def threeDim() = Array.fill[Option[I]](1, 1, size, size, size)(None)

    def getHelper(nonterm: Nonterm) =
      Accessor(v(nonterm.id)(nonterm.d)(nonterm.m)(nonterm.i)(nonterm.j), nonterm.k)

    val isOK:(Nonterm=>Boolean) =
      if (maxDepth == 0) _ => true
      else x => x.d < d && x.d >= 0
  }
}

trait LeftCornerSplitHeadViterbiParser[Param]
    extends LeftCornerSplitHeadBasicParser[Param] with ViterbiParser {

  type Item = Option[Child]
  type Table = BasicTable[Item]

  sealed trait Child { def score: Double }
  case class Unary(child: Nonterm, score: Double) extends Child
  case class Binary(left: Nonterm, right: Nonterm, score: Double) extends Child
  case class Terminal(score: Double) extends Child

  import nonterms._

  def mkTable(n: Int) = new BasicTable[Option[Child]] {
    def size = n
    def blankItem = None
  }

  def mkChart(sentence: Sentence, partial: Param, inside: Table) =
    new ViterbiChart(sentence, partial, inside)

  def parse(sentence: Sentence): Option[(DepArcs, Double)] = {
    val inside = mkTable(sentence.size)
    val partial: Param = mkPartial(sentence.size)

    val chart = mkChart(sentence, partial, inside)
    parseHelper(sentence.size, chart)
  }

  private def parseHelper(n: Int, chart: ViterbiChart): Option[(DepArcs, Double)] = {
    val heads = Array.fill(n)(0)

    def addArcs(node: Nonterm): Unit = chart.bestChild(node).foreach { best =>
      (node, best) match {
        // only catch rules which add new arcs
        case (S(b, h, e), Unary(child, _)) if b == 0 && e == n - 1 =>
          heads(h) = -1 // root
          addArcs(child)
        case (SquareFullPred(b, e, p, _), Unary(triangle: Triangle, _)) =>
          // left-pred
          heads(triangle.j) = p
          addArcs(triangle)
        case (RightFullPred(h, e, p, _, _), Unary(rightFull: RightFull, _)) =>
          // right-pred
          heads(p) = h
          addArcs(rightFull)
        case (SquareFragPred(s, h, p, _, _),
          Binary(left: SquareFullPred, right: LeftFull, _)) =>
          // left-comp
          heads(right.j) = p
          addArcs(left)
          addArcs(right)
        case (RightFragPred(s, h, p, _, _),
          Binary(left: RightFullPred, right: LeftFull, _)) =>
          heads(right.j) = p
          addArcs(left)
          addArcs(right)
        case (RightFullPred(h, e, p, _, _),
          Binary(left: RightFullPred, right: RightFull, _)) =>
          assert(left.k == left.j + 1 && left.k == right.i)
          heads(p) = left.k
          addArcs(left)
          addArcs(right)
        case (_, Terminal(_)) =>
        case (_, Unary(node, _)) =>
          addArcs(node)
        case (_, Binary(left, right, _)) =>
          addArcs(left)
          addArcs(right)
      }
    }
    chart.rootIdx match {
      case -1 => None
      case root =>
        chart.bestChild(S(0, root, n - 1)).map(_.score).map {
          case 0 => None
          case score =>
            addArcs(S(0, root, n - 1))
            Some((NormalDepArcs(heads), score))
        }.getOrElse(None)
    }
  }

  class ViterbiChart(
    val sentence: Sentence,
    val partial: Param,
    val insideTable: Table) extends Chart {

    val outsideTable = null

    lazy val rootIdx: Int = sampleArgmax((0 until n).flatMap { h =>
      bestChild(S(0, h, n - 1))
    })._2

    def incrementCounts(): Unit =
      sys.error("not implemented yet (probably would be used in viterbi learning)")

    def marginal() = bestChild(S(0, rootIdx, n - 1)).map(_.score).getOrElse(0.0)

    def terminalOutside(i: Int) = sys.error("unsupported error")

    def bestChild(node: Nonterm): Option[Child] = insideTable.getOrUpdate(node) {
      case S(b, h, e) =>
        val child = Triangle(b, h, e, 0)
        bestChild(child).map { best =>
          Unary(child, best.score * rules.selectRoot(h).score)
        }
      case Triangle(b, h, e, d) =>
        val left = LeftFull(b, h, d)
        val right = RightFull(h, e, d)
        for (
          bestLeft <- bestChild(left);
          bestRight <- bestChild(right)
        ) yield Binary(left, right, bestLeft.score * bestRight.score)
      case LeftFull(i, j, d) if i == j =>
        Some(Terminal(rules.initiateLeftHalf(i).score))
      case LeftFull(i, j, d) =>
        val child = SquareFullPred(i, j - 1, j, d)
        bestChild(child) map { b => Unary(child, b.score * 1.0) }
      case RightFull(i, j, d) if i == j =>
        Some(Terminal(1.0))
      case RightFull(i, j, d) =>
        val child = RightFullPred(i, j - 1, j, d)
        bestChild(child) map { b => Unary(child, b.score * 1.0) }
      case SquareFullPred(b, e, p, d) =>
        val leftPreds: Seq[Child] = for (
          h:Int <- b to e;
          child = Triangle(b, h, e, d);
          best <- bestChild(child)
        ) yield Unary(child, best.score * rules.leftPred(b, h, e, p).score)

        val leftComps: Seq[Child] = for (
          h <- b + 1 to e;
          m <- depthCalc.possibleLeftChunkSizes(b, h);
          left = SquareFragPred(b, h, p, d, m);
          right = rightFullForComp(h, e, d, m);
          bestLeft <- bestChild(left);
          bestRight <- bestChild(right)
        ) yield Binary(left,
          right,
          bestLeft.score * bestRight.score * rules.leftCompSecond(b, h, e, p).score)

        sampleArgmax(leftPreds ++ leftComps)._1
      case SquareFragPred(b, e, p, d, m) =>
        val cands = for (
          segment <- depthCalc.possibleLeftCompSegments(b, e, m).toSeq;
          left = SquareFullPred(b, segment, p, d);
          right = LeftFull(segment + 1, e, d);
          bestLeft <- bestChild(left);
          bestRight <- bestChild(right)
        ) yield Binary(left,
          right,
          bestLeft.score * bestRight.score * rules.leftCompFirst(b, segment, e, p).score)

        sampleArgmax(cands)._1
      case RightFullPred(h, e, p, d, _) =>
        val rightPreds = {
          val child = RightFull(h, e, d)
          val cands = for (b <- bestChild(child)) yield
            Unary(child, b.score * rules.rightPred(h, e, p).score)
          cands.map(Seq(_)).getOrElse(Seq())
        }
        val leftComps = for (
          segment <- h + 1 to e;
          m <- depthCalc.possibleLeftChunkSizes(h, segment);
          left = RightFragPred(h, segment, p, d, m);
          right = rightFullForComp(segment, e, d, m);
          bestLeft <- bestChild(left);
          bestRight <- bestChild(right)
        ) yield Binary(left,
          right,
          bestLeft.score * bestRight.score * rules.leftCompSecond(h, segment, e, p).score)
        val rightComps = for (
          segment <- h to e - 1;
          left = RightFullPred(h, segment, segment + 1, d);
          right = rightFullForComp(segment + 1, e, d, 0);
          bestLeft <- bestChild(left);
          bestRight <- bestChild(right)
        ) yield Binary(left,
          right,
          bestLeft.score * bestRight.score * rules.rightComp(h, segment, e, p).score)

        sampleArgmax(rightPreds ++ leftComps ++ rightComps)._1
      case RightFragPred(h, e, p, d, m) =>
        val cands = for (
          segment <- depthCalc.possibleLeftCompSegments(h, e, m).toSeq;
          left = RightFullPred(h, segment, p, d);
          right = LeftFull(segment + 1, e, d);
          bestLeft <- bestChild(left);
          bestRight <- bestChild(right)
        ) yield Binary(left,
          right,
          bestLeft.score * bestRight.score * rules.leftCompFirst(h, segment, e, p).score)

        sampleArgmax(cands)._1
    }

    private def sampleArgmax(children: Seq[Child]): (Option[Child], Int) =
      if (children.isEmpty) (None, -1)
      else {
        import util.Random

        val maxScores = children.zipWithIndex.groupBy(x=>x._1.score).maxBy(_._1)._2
        val idx = if (maxScores.size > 1) Random.nextInt(maxScores.size) else 0
        (Some(maxScores(idx)._1), maxScores(idx)._2)
      }
  }
}

trait LeftCornerSplitHeadExpectingParser[Param]
    extends LeftCornerSplitHeadBasicParser[Param] with ExpectingParser[Param] {

  type Item = Double
  type Table = BasicTable[Item]

  import nonterms._

  def mkTable(n: Int) = new BasicTable[Double] {
    def size = n
    def blankItem = 0
  }

  def mkChart(sentence: Sentence, partial: Param, inside: Table, outside: Table) =
    new ExpectingChart(sentence, partial, inside, outside)

  class ExpectingChart(
    val sentence: Sentence,
    val partial: Param,
    val insideTable: Table,
    val outsideTable: Table) extends Chart {

    def marginal() = (0 to n - 1).map { r => insideScore(S(0, r, n - 1)) }.sum

    def insideScore(nonterm: Nonterm): Double = insideTable.getOrUpdate(nonterm) {
      case S(b, h, e) =>
        assert(b == 0 && e == n - 1)
        insideScore(Triangle(b, h, e, 0)) * rules.selectRoot(h).score
      case Triangle(i, j, k, d) =>
        insideScore(LeftFull(i, j, d)) * insideScore(RightFull(j, k, d)) * 1.0
      case LeftFull(i, j, d) if i == j => rules.initiateLeftHalf(i).score
      case LeftFull(i, j, d) => insideScore(SquareFullPred(i, j - 1, j, d)) * 1.0
      case RightFull(i, j, d) if i == j => 1.0
      case RightFull(i, j, d) =>
        insideScore(RightFullPred(i, j - 1, j, d)) * rules.insertRightPred(j).score
      case SquareFullPred(b, e, p, d) =>
        var sum = 0.0
        // leftPredScore
        (b to e) foreach { h =>
          sum += insideScore(Triangle(b, h, e, d)) * rules.leftPred(b, h, e, p).score
        }
        // leftCompScore
        (b + 1 to e) foreach { h =>
          depthCalc.possibleLeftChunkSizes(b, h) foreach { m =>
            sum += insideScore(SquareFragPred(b, h, p, d, m)) *
            insideScore(rightFullForComp(h, e, d, m)) *
            rules.leftCompSecond(b, h, e, p).score
          }
        }
        sum
      case SquareFragPred(b, e, p, d, m) =>
        var sum = 0.0
        depthCalc.possibleLeftCompSegments(b, e, m) foreach { segment =>
          sum += insideScore(SquareFullPred(b, segment, p, d)) *
          insideScore(LeftFull(segment + 1, e, d)) * // we do not increment depth at first step of leftcomp
          rules.leftCompFirst(b, segment, e, p).score
        }
        sum
      case RightFullPred(h, e, p, d, _) =>
        var sum = 0.0
        sum += insideScore(RightFull(h, e, d)) * rules.rightPred(h, e, p).score // rightPred

        // leftComp
        (h + 1 to e) foreach { segment =>
          depthCalc.possibleLeftChunkSizes(h, segment) foreach { m =>
            sum += insideScore(RightFragPred(h, segment, p, d, m)) *
            insideScore(rightFullForComp(segment, e, d, m)) *
            rules.leftCompSecond(h, segment, e, p).score
          }
        }
        // rightComp
        (h to e - 1) foreach { segment =>
          sum += insideScore(RightFullPred(h, segment, segment + 1, d)) *
          insideScore(rightFullForComp(segment + 1, e, d, 0)) *
          rules.rightComp(h, segment, e, p).score
        }
        sum
      case RightFragPred(h, e, p, d, m) =>
        var sum = 0.0
        depthCalc.possibleLeftCompSegments(h, e, m) foreach { segment =>
          sum += insideScore(RightFullPred(h, segment, p, d)) *
          insideScore(LeftFull(segment + 1, e, d)) *
          rules.leftCompFirst(h, segment, e, p).score
        }
        sum
    }

    def outsideScore(nonterm: Nonterm): Double = outsideTable.getOrUpdate(nonterm) {
      case S(b, h, e) =>
        assert(b == 0 && e == n - 1)
        1.0
      case Triangle(b, h, e, d) =>
        var sum = 0.0

        // leftPred
        (e + 1 to n - 1) foreach { p =>
          sum += outsideScore(SquareFullPred(b, e, p, d)) *
          rules.leftPred(b, h, e, p).score
        }
        val rootScore =
          if (b != 0 || e != n - 1 || d != 0) 0.0
          else outsideScore(S(b, h, e)) * rules.selectRoot(h).score
        sum + rootScore
      case LeftFull(e, h, d) =>
        var sum = 0.0
        // combine
        (h to n - 1) foreach { rightEdge =>
          sum += outsideScore(Triangle(e, h, rightEdge, d)) *
          insideScore(RightFull(h, rightEdge, d)) * 1.0
        }
        // leftComp
        (0 to e - 1) foreach { leftBegin =>

          // val chunkSize = depthCalc.leftChunkSize(h - e)
          val chunkSize = depthCalc.leftChunkSize(e, h)
          // val leftHandDepth = if (notIncrease(h, e)) d else d - 1
          (h + 1 to n - 1) foreach { p =>
            val localScore = rules.leftCompFirst(leftBegin, e - 1, h, p).score
            sum += outsideScore(SquareFragPred(leftBegin, h, p, d, chunkSize)) *
            insideScore(SquareFullPred(leftBegin, e - 1, p, d)) *
            localScore
            sum += outsideScore(RightFragPred(leftBegin, h, p, d, chunkSize)) *
            insideScore(RightFullPred(leftBegin, e - 1, p, d)) *
            localScore
          }
        }
        sum
      case RightFull(h, e, d) =>
        var sum = 0.0
        // combine
        (0 to h) foreach { leftEdge =>
          sum += outsideScore(Triangle(leftEdge, h, e, d)) * insideScore(LeftFull(leftEdge, h, d)) * 1.0
        }

        // rightPred
        (e + 1 to n - 1) foreach { p =>
          sum += outsideScore(RightFullPred(h, e, p, d)) * rules.rightPred(h, e, p).score
        }

        // val leftHandDepth = if (notIncrease(h, e)) d else d - 1

        // leftComp
        (0 to h - 1) foreach { leftBegin =>
          depthCalc.possibleLeftChunkSizes(leftBegin, h) foreach { m =>
            (e + 1 to n - 1) foreach { p =>
              val localScore = rules.leftCompSecond(leftBegin, h, e, p).score
              // val leftHandDepth = d - depthCalc.compDepthDiff(m, e - h)
              val leftHandDepth = d - depthCalc.compDepthDiff(m, h, e)

              sum += outsideScore(SquareFullPred(leftBegin, e, p, leftHandDepth)) *
              insideScore(SquareFragPred(leftBegin, h, p, leftHandDepth, m)) * localScore
              sum += outsideScore(RightFullPred(leftBegin, e, p, leftHandDepth)) *
              insideScore(RightFragPred(leftBegin, h, p, leftHandDepth, m)) * localScore
            }
          }
        }

        // rightComp
        // val leftHandDepth = d - depthCalc.compDepthDiff(e - h)
        val leftHandDepth = d - depthCalc.rightCompDepthDiff(h, e)
        (0 to h - 1) foreach { leftHead =>
          (e + 1 to n - 1) foreach { p =>
            sum += outsideScore(RightFullPred(leftHead, e, p, leftHandDepth)) *
            insideScore(RightFullPred(leftHead, h - 1, h, leftHandDepth)) *
            rules.rightComp(leftHead, h - 1, e, p).score
          }
        }
        sum
      case SquareFullPred(b, e, p, d) =>
        var sum = 0.0
        if (p == e + 1) sum += outsideScore(LeftFull(b, e + 1, d)) * 1.0 // insert

        // leftComp
        (e + 1 to p - 1) foreach { rightHead =>
          // sum += outsideScore(SquareFragPred(b, rightHead, p, d, depthCalc.leftChunkSize(rightHead - (e + 1)))) *
          sum += outsideScore(SquareFragPred(b, rightHead, p, d, depthCalc.leftChunkSize(e + 1, rightHead))) *
          insideScore(LeftFull(e + 1, rightHead, d)) *
          rules.leftCompFirst(b, e, rightHead, p).score
        }
        sum
      case SquareFragPred(b, e, p, d, m) =>
        var sum = 0.0
        (e to p - 1) foreach { rightEdge =>
          sum += outsideScore(SquareFullPred(b, rightEdge, p, d)) *
          insideScore(rightFullForComp(e, rightEdge, d, m)) *
          rules.leftCompSecond(b, e, rightEdge, p).score
        }
        sum
      case RightFullPred(h, e, p, d, _) =>
        var sum = 0.0
        // insert
        if (p == e + 1)
          sum += outsideScore(RightFull(h, e + 1, d)) * rules.insertRightPred(e + 1).score

        // leftComp
        (e + 1 to p - 1) foreach { rightHead =>
          // sum += outsideScore(RightFragPred(h, rightHead, p, d, depthCalc.leftChunkSize(rightHead - (e + 1)))) *
          sum += outsideScore(RightFragPred(h, rightHead, p, d, depthCalc.leftChunkSize(e + 1, rightHead))) *
          insideScore(LeftFull(e + 1, rightHead, d)) *
          rules.leftCompFirst(h, e, rightHead, p).score
        }

        // rightComp
        if (p == e + 1) (e + 1 to n - 2) foreach { rightEdge =>
          val inside = insideScore(rightFullForComp(e + 1, rightEdge, d, 0))
          // RightFull.forComp(e + 1, rightEdge, d)
          (rightEdge + 1 to n - 1) foreach { newPred =>
            sum += outsideScore(RightFullPred(h, rightEdge, newPred, d)) *
            inside *
            rules.rightComp(h, e, rightEdge, newPred).score
          }
        }
        sum
      case RightFragPred(h, e, p, d, m) =>
        var sum = 0.0
        (e to p - 1) foreach { rightEdge =>
          sum += outsideScore(RightFullPred(h, rightEdge, p, d)) *
          insideScore(rightFullForComp(e, rightEdge, d, m)) *
          rules.leftCompSecond(h, e, rightEdge, p).score
        }
        sum
    }

    def terminalOutside(idx: Int): Double = {
      val leftInsertScore = candDepth().flatMap { d =>
        (0 to idx - 1).map { s =>
          outsideScore(LeftFull(s, idx, d)) * insideScore(SquareFullPred(s, idx - 1, idx, d)) * 1.0
        }
      }.sum
      val initialRightInsertScore = candDepth().flatMap { d =>
        (0 to idx - 1).map { h =>
          outsideScore(RightFull(h, idx, d)) *
          insideScore(RightFullPred(h, idx - 1, idx, d)) *
          rules.insertRightPred(idx).score
        }
      }.sum
      val rightCompScore = candDepth().flatMap { d =>
        (0 to idx - 1).flatMap { h =>
          (idx to n - 2).flatMap { e =>
            (e + 1 to n - 1).map { p =>
              outsideScore(RightFullPred(h, e, p, d)) *
              insideScore(RightFullPred(h, idx - 1, idx, d)) *
              insideScore(rightFullForComp(idx, e, d, 0)) *
              rules.rightComp(h, idx - 1, e, p).score
            }
          }
        }
      }.sum
      val leftFullScore = candDepth().map { d =>
        outsideScore(LeftFull(idx, idx, d)) * rules.initiateLeftHalf(idx).score
      }.sum
      val leftHalfScore = leftFullScore + leftInsertScore + initialRightInsertScore + rightCompScore

      val secondRightInsertScore = candDepth().flatMap { d =>
        (0 to idx - 1).map { h =>
          outsideScore(RightFull(h, idx, d)) *
          insideScore(RightFullPred(h, idx - 1, idx, d)) *
          rules.insertRightPred(idx).score
        }
      }.sum
      val rightFullScore = candDepth().map { d =>
        outsideScore(RightFull(idx, idx, d))
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
      for (rootIdx <- 0 to n - 1) {
      val rule = rules.selectRoot(rootIdx)
        rule.increment(
          insideScore(Triangle(0, rootIdx, n - 1, 0)) *
            outsideScore(S(0, rootIdx, n - 1)) *
            rule.score)
      }

      candDepth() foreach { d =>
        // initial left half
        for (idx <- 0 to n - 1) {
          val rule = rules.initiateLeftHalf(idx)
          val outside = outsideScore(LeftFull(idx, idx, d))
          if (outside > 0) rule.increment(outside * rule.score)
        }
        // right insert
        for (
          headIdx <- 0 to n - 1;
          edgeIdx <- headIdx to n - 2) {

          val insertedIdx = edgeIdx + 1
          val rule = rules.insertRightPred(insertedIdx) // NOTE: this is ok
          rule.increment(insideScore(RightFullPred(headIdx, edgeIdx, insertedIdx, d)) *
            outsideScore(RightFull(headIdx, edgeIdx + 1, d)) *
            rule.score)
        }
        // left pred
        for (
          begin <- 0 to n - 2;
          end <- begin to n - 2;
          headIdx <- begin to end;
          inside = insideScore(Triangle(begin, headIdx, end, d));
          if inside > 0;
          predIdx <- end + 1 to n - 1;
          outside = outsideScore(SquareFullPred(begin, end, predIdx, d));
          if outside > 0) {

          val rule = rules.leftPred(begin, headIdx, end, predIdx)
          rule.increment(inside * outside * rule.score)
        }
        // right pred
        for (
          headIdx <- 0 to n - 2;
          edgeIdx <- headIdx to n - 2;
          inside = insideScore(RightFull(headIdx, edgeIdx, d));
          if inside > 0;
          predIdx <- edgeIdx + 1 to n - 1;
          outside = outsideScore(RightFullPred(headIdx, edgeIdx, predIdx, d));
          if outside > 0) {

          val rule = rules.rightPred(headIdx, edgeIdx, predIdx)
          rule.increment(inside * outside * rule.score)
        }
        // left comp (1st part)
        for (
          begin <- 0 to n - 2;
          newEnd <- begin + 1 to n - 2;
          predIdx <- newEnd + 1 to n - 1;
          segment <- begin to newEnd - 1;
          // m = depthCalc.leftChunkSize(newEnd - (segment + 1));
          m = depthCalc.leftChunkSize(segment + 1, newEnd);
          squareOutside = outsideScore(SquareFragPred(begin, newEnd, predIdx, d, m));
          rightOutside = outsideScore(RightFragPred(begin, newEnd, predIdx, d, m));
          if squareOutside > 0 || rightOutside > 0) {

          val rule = rules.leftCompFirst(begin, segment, newEnd, predIdx)
          val leftFullInside = insideScore(LeftFull(segment + 1, newEnd, d))

          if (leftFullInside > 0) {
            if (squareOutside > 0) {
              val squareInside = insideScore(SquareFullPred(begin, segment, predIdx, d))
              rule.increment(squareInside * leftFullInside * squareOutside * rule.score)
            }
            if (rightOutside > 0) {
              val rightFragInside = insideScore(RightFullPred(begin, segment, predIdx, d))
              rule.increment(rightFragInside * leftFullInside * rightOutside * rule.score)
            }
          }
        }
        // left comp (2nd part)
        for (
          begin <- 0 to n - 2;
          newEnd <- begin + 1 to n - 2;
          predIdx <- newEnd + 1 to n - 1;
          squareOutside = outsideScore(SquareFullPred(begin, newEnd, predIdx, d));
          rightOutside = outsideScore(RightFullPred(begin, newEnd, predIdx, d));
          if squareOutside > 0 || rightOutside > 0;
          segment <- begin + 1 to newEnd;
          rule = rules.leftCompSecond(begin, segment, newEnd, predIdx);
          m <- depthCalc.possibleLeftChunkSizes(begin, segment);
          rightFullInside = insideScore(rightFullForComp(segment, newEnd, d, m));
          if rightFullInside > 0) {

          if (squareOutside > 0) {
            val squareInside = insideScore(SquareFragPred(begin, segment, predIdx, d, m))
            rule.increment(squareInside * rightFullInside * squareOutside * rule.score)
          }
          if (rightOutside > 0) {
            val rightFragInside = insideScore(RightFragPred(begin, segment, predIdx, d, m))
            rule.increment(rightFragInside * rightFullInside * rightOutside * rule.score)
          }
        }
        // right comp
        for (
          begin <-0 to n - 3;
          end <- begin + 1 to n - 2;
          segment <- begin to end - 1;
          rightFullInside = insideScore(rightFullForComp(segment + 1, end, d, 0))
          if rightFullInside > 0;
          rightFragInside = insideScore(RightFullPred(begin, segment, segment + 1, d));
          if rightFragInside > 0;
          predIdx <- end + 1 to n - 1) {

          val outside = outsideScore(RightFullPred(begin, end, predIdx, d))

          val rule = rules.rightComp(begin, segment, end, predIdx)
          rule.increment(rightFragInside * rightFullInside * outside * rule.score)
        }
      }
      paramHolder.normalizePartial(marginal)
    }
  }
}
