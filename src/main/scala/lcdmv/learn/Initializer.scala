package lcdmv.learn

import breeze.linalg.DenseVector
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize._
import collection.GenTraversable
import lcdmv.data.{DepArcs, Sentence}
import lcdmv.model.{DMVFeatureHolder, DMVParameter, ExpectedCounts, ExpectingParser, ParamHolder, PartialCounts}

class FeatureInitializer[Param<:DMVParameter](
  counts: Param,
  featureHolder: DMVFeatureHolder) {

  def optimizedParamAndWeight(
    param: Param,
    sentences: IndexedSeq[Sentence],
    regularization: Double = 10.0): DenseVector[Double] = {

    val initialWeight = DenseVector.zeros[Double](featureHolder.numFeatures)

    val obj = DiffFunction.withL2Regularization(
      new EMObjective(counts, featureHolder), regularization)
    val weight = new LBFGS[DenseVector[Double]]().minimize(obj, initialWeight)

    initParam(param, weight)

    weight
  }

  def unoptimizedParamAndWeight(param: Param): DenseVector[Double] = {
    param.clear
    param += counts
    param.normalize

    DenseVector.zeros[Double](featureHolder.numFeatures)
  }

  private def initParam(param: Param, weight: DenseVector[Double]) = {
    param.clear
    param.addFeatureWeights(featureHolder, weight)
    param.normalize
  }
}

class CountCollector[Param<:DMVParameter](
  sentences: IndexedSeq[Sentence],
  token: Sentence=>Int=>Int,
  mkEmpty: ()=>Param,
  mkPartial: Int=>PartialCounts = new PartialCounts(_)) {

  def collectEMHarmonic(parser: ExpectingParser[Param]): Param =
    ExpectedCounts.collect(
      parser,
      learn.makePar(sentences.grouped(sentences.size / 20).toIndexedSeq, -1)(0 until 20)
    ).counts

  def collectUniform() = {
    val x = mkEmpty()
    x.initUniform
    x
  }

  def collectRandom() = {
    val x = mkEmpty()
    x.initRandom()
    x
  }

  def collectSupervise(arcs: Seq[DepArcs]) = {
    require(sentences.size == arcs.size)
    val counts = mkEmpty()

    sentences.zip(arcs) foreach { case (s, g) =>
      val partial = mkPartial(s.size)
      partial.addSupervisedCounts(g)
      counts.addPartial(partial, token(s))
    }
    counts
  }
}
