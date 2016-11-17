package lcdmv.learn

import collection.GenTraversable
import com.typesafe.scalalogging.slf4j._
import breeze.linalg.DenseVector
import breeze.optimize.BatchDiffFunction
import lcdmv.data.{Sentence}
import lcdmv.model.{DMVFeatureHolder, ExpectedCounts, ExpectingParser, DMVParameter, ParamHolder}

/** For the time being, this objective only supports direct gradient method (not EM)
  *
  */
class Objective[Param<:DMVParameter](
  val parser: ExpectingParser[Param],
  val featureHolder: DMVFeatureHolder,
  batchSelector: IndexedSeq[Int]=>GenTraversable[Seq[Sentence]],
  val fullRange: IndexedSeq[Int],
  val observingFeatures: Seq[(String, Int)]
) extends BatchDiffFunction[DenseVector[Double]] with StrictLogging {

  val sentences = batchSelector(fullRange)

  def this(parser: ExpectingParser[Param],
    featureHolder: DMVFeatureHolder,
    data: IndexedSeq[Sentence],
    observingFeatures: Seq[(String, Int)],
    numThreads: Int = -1) =
    this(
      parser,
      featureHolder,
      { x: IndexedSeq[Int] => learn.parGroups(x.map(data), 20, numThreads) },
      0 until data.size,
      observingFeatures)

  protected def select(batch: IndexedSeq[Int]): GenTraversable[Seq[Sentence]] =
    batchSelector(batch)

  var iter = 0

  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {

    // WARNING: We assume parameter at first iteration is initialized outside
    if (iter != 0) normalizeWith(x)

    val e: ExpectedCounts[Param] = ExpectedCounts.collect(parser, select(batch))

    val gradient = parser.param.gradient(featureHolder, e.counts)
    gradient :*= -1.0

    val loss = -e.loss

    println(s"loss:${iter}:${e.loss}")
    iter += 1

    (loss * fullRange.size / batch.size, gradient * (fullRange.size * 1.0 / batch.size))
  }

  def normalizeWith(x: DenseVector[Double]) = {
    parser.param.clear
    parser.param.addFeatureWeights(featureHolder, x)
    parser.param.normalize
  }
}

// optimize param to fit to expected counts
class EMObjective[Param<:DMVParameter](counts: Param, featureHolder: DMVFeatureHolder)
    extends BatchDiffFunction[DenseVector[Double]] with StrictLogging {

  def fullRange = IndexedSeq(0)

  val param = counts.empty

  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {

    normalizeWith(x)

    val loss = -param.logLikelihood(counts)

    val gradient = param.gradient(featureHolder, counts)
    gradient :*= -1.0

    (loss, gradient)
  }

  def normalizeWith(x: DenseVector[Double]) = {
    param.clear
    param.addFeatureWeights(featureHolder, x)
    param.normalize
  }
}
