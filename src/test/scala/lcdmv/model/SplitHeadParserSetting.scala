package lcdmv.model

import lcdmv.const.{Direction, Valence}
import lcdmv.data.{DepArcs, NormalDepArcs, Sentence, SimpleWord, SimplePoS}
import org.scalatest._

trait SplitHeadParserSetting extends Matchers {

  val one = 1.0 +- 1e-8
  val two = 2.0 +- 1e-8

  def Left = Direction.left
  def Right = Direction.right
  def NoChild = Valence.noChild
  def HasChild = Valence.hasChild

  def equalParams(left: DMVParameter, right: DMVParameter) = {
    val error = 1e-8

    left.size should equal (right.size)
    left.foreachGen { case (h, c, gen) =>
      gen.stop should equal (right.gen(h, c).stop +- error)
      gen.proceed should equal (right.gen(h, c).proceed +- error)
    }
    left.foreachChoice { case (h, d, choice) =>
      choice.activeKeys foreach { a =>
        choice(a) should equal (right.choice(h, d)(a) +- error)
      }
    }
    left.root.activeKeys foreach { i =>
      left.root(i) should equal (right.root(i) +- error)
    }
  }

  class BiasedParserGen(size: Int) { outer =>

    val param = new NaiveDMVParameter(size)

    def setBiasedParam(activeLinks: (Int, Int)*): Unit = {
      param.initUniform
      param.foreachChoice { (_,_,a) => a.map(_=>0.0) }

      activeLinks foreach { case (head, dep) =>
        val dir = if (head > dep) Left else Right
        param.incrementChoice(head, dir, dep, 1.0)
      }
    }
    def setUniformParam(): Unit = {
      param.initUniform
      param.normalize
    }
    def addLinkWeight(head: Int, dep: Int, w: Double = 0.5): Unit = {
      val dir = if (head > dep) Left else Right
      param.incrementChoice(head, dir, dep, w)
    }

    val sentence = new Sentence {
      private val alphabet = "abcdefghijklmnopqrstuvwxyz"
      private val p = (0 until size).map(i=>SimplePoS(i, alphabet(i)+"", alphabet(i)+""))
      def word(i: Int) = SimpleWord(0, "a")
      def pos(i: Int) = p(i)
      def size = outer.size
    }

    def parser(_maxDepth: Int, _depthCalc: SplitHeadDepthCalculator) =
      new NaiveDMVParamParser(param, _maxDepth, _depthCalc, null)
          with LeftCornerSplitHeadExpectingParser[DMVParameter]

    def viterbiParser(_maxDepth: Int, _depthCalc: SplitHeadDepthCalculator) =
      new NaiveDMVParamParser(param, _maxDepth, _depthCalc, null)
          with LeftCornerSplitHeadViterbiParser[DMVParameter]

    def condParser(linkChecker: LinkSatisfactionChecker) =
      new NaiveDMVParamParser(param, 0, UnlimitedDepthCalculator, linkChecker)
          with LeftCornerSplitHeadExpectingCondParser[DMVParameter]

    def leafCondParser(leafCond: LeafCondCalculator) =
      new NaiveDMVParamParser(param, 0, UnlimitedDepthCalculator, UnrestrictedLinkChecker, leafCond)
          with LeftCornerSplitHeadExpectingCondParser[DMVParameter]
  }
}
