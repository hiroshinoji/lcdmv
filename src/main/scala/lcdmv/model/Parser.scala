package lcdmv.model

import lcdmv.const.{Direction, Valence}
import lcdmv.data.{DepArcs, NormalDepArcs, Sentence}

import scala.reflect.ClassTag

/** An example usage of Parser, which should be mix-ined with another concrete parser,
  * such as LeftCornerSplitHeadExpectingParser.
  * Useful in unit tests.
  */
abstract class NaiveDMVParamParser(
  val param: DMVParameter,
  val maxDepth: Int,
  val depthCalc: SplitHeadDepthCalculator,
  val cond: LinkSatisfactionChecker,
  val leafCond: LeafCondCalculator = NoLeafCondCalculator)
    extends Parser[DMVParameter] { outer =>

  def mkPartial(n: Int) = new PartialCounts(n)
  def mkEmptyParam = param.empty
  def mkParamHolder(_sentence: Sentence, _partial: DMVParameter) = new DMVParamHolder {
    val sentence = _sentence
    val token: (Int=>Int) = sentence.pos(_).id
    val param = outer.param
    val partial = _partial

    val distanceCost = ParamHolder.NoneDistanceCostCalculator
    val proceedCost = ParamHolder.NoProceedCostCalculator
    val compCost = ParamHolder.NoneCompCostCalculator
    val rootPOSConstraint = ParamHolder.NoRootConstrait
    val linkPOSConstraint = ParamHolder.NoLinkPOSConstraint
  }

  def mkDepthCalculator(sentence: Sentence) = depthCalc
  def calcMaxDepth(n: Int) = maxDepth
  def calcMaxChunkSize(n: Int) = depthCalc.maxChunkSize
  def mkCondition(sentence: Sentence) = cond
  def mkLeafCondition(sentence: Sentence) = leafCond
}

/** Parser is actually a generator and manager of chart.
  * Each parser class must implement mkChart, which returns a new chart used in expectation or viterbi parse.
  */
trait Parser[Param] { outer =>

  type Item
  type Nonterm
  type Table <: ChartTable[_, _]

  val nonterms = LeftCornerSplitHeadNonterminals

  val param: Param

  def mkPartial(n: Int): Param
  def mkEmptyParam(): Param
  def mkParamHolder(sentence: Sentence, partial: Param): ParamHolder[Param]

  /** These constraints are defined as a function of sentence which are expected to be defined in the interface (Problem.scala). (User does not customize ChartImpl)
    */
  // def mkDepthCalculator(n: Int): SplitHeadDepthCalculator
  def mkDepthCalculator(sentence: Sentence): SplitHeadDepthCalculator
  def calcMaxDepth(n: Int): Int
  def calcMaxChunkSize(n: Int): Int
  def mkCondition(sentence: Sentence): LinkSatisfactionChecker
  def mkLeafCondition(sentence: Sentence): LeafCondCalculator
  // def mk

  protected def mkTable(n: Int): Table

  trait Chart {
    val sentence: Sentence
    val partial: Param
    val insideTable: Table
    val outsideTable: Table

    val paramHolder = mkParamHolder(sentence, partial)
    val depthCalc = mkDepthCalculator(sentence)
    val maxDepth = calcMaxDepth(sentence.size)
    val condition = mkCondition(sentence) // this is only used in CondParser
    val leafCond = mkLeafCondition(sentence)

    assert(maxChunkSize == depthCalc.maxChunkSize)

    // def maxChunkSize = depthCalc.maxChunkSize
    def maxChunkSize = calcMaxChunkSize(sentence.size)
    def n = sentence.size
    def rules = paramHolder.rules

    def marginal(): Double

    def terminalOutside(i: Int): Double

    def incrementCounts(): Unit
    // def partialCount = paramHolder.partial

    protected def rightFullForComp(head: Int, edge: Int, d: Int, leftChunk: Int) = {
      // nonterms.RightFull(
      //   head, edge, d + depthCalc.compDepthDiff(leftChunk, edge - head))
      nonterms.RightFull(
        head, edge, d + depthCalc.compDepthDiff(leftChunk, head, edge))
    }
  }

  trait ChartTable[N, @specialized(Double) I] {
    def size: Int

    /** These two values are specified as an upperbound of actual limit.
      * (size would be the maximum sentence length across the sentence group)
      */
    lazy val maxDepth = calcMaxDepth(size)
    lazy val maxChunkSize = calcMaxChunkSize(size) // mkDepthCalculator(size).maxChunkSize

    def clearCounts(n: Int): Unit

    /** None element means that chart has not been visited yet
      * while blankItem indicates that element should be empty.
      */
    def blankItem: I

    def apply(nonterm: N): Option[I] =
      getHelper(nonterm) match { case Accessor(array, i) => array(i) }

    protected def getHelper(nonterm: N): Accessor

    /** The reason why we prepare this case class is Tuple2 with no specialized object
      * would cause boxing even another type is specilized (in this case Int).
      */
    case class Accessor(array: Array[Option[I]], var k: Int)

    /** maxDepth might be changed according to the sentence length so the condition whether that nonterminal is ok should be determined by some function (not by the boundary check of chart table).
      */
    val isOK: (N=>Boolean)

    def getOrUpdate(nonterm: N)(calc: N => I) =
      if (!isOK(nonterm)) blankItem
      else getHelper(nonterm) match {
        case Accessor(a, k) =>
          a(k) getOrElse {
            a(k) = Some(calc(nonterm))
            a(k).get
          }
      }
  }
}
