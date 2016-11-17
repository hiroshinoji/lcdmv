package lcdmv.learn

import breeze.linalg.DenseVector
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize._
import breeze.linalg.DenseVector
import lcdmv.data.{Sentence}
import lcdmv.model.{DMVFeatureHolder, DMVParameter, ExpectingParser, ParamHolder}
import scala.collection.GenTraversable

class DirectGradientTrainer[Param<:DMVParameter](
  val parser: ExpectingParser[Param],
  val featureHolder: DMVFeatureHolder,
  val sentences: IndexedSeq[Sentence],
  numThreads: Int = -1) {

  def run(
    opt: OptParams,
    initWeight: DenseVector[Double],
    observingFeatures: Seq[(String, Int)]) = {

    // val counts = new ExpectationCalculator(mkParamHolder, parser, token)
    //   .collectCounts(param, learn.makePar(sentences, -1)(0 until sentences.size))
    //   .counts
    // val firstWeight = new LBFGS[DenseVector[Double]]().minimize(
    //   DiffFunction.withL2Regularization(
    //     new EMObjective(counts, featureHolder), opt.regularization),
    //   initWeight)

    // param.clear
    // param.addFeatureWeights(featureHolder, firstWeight)
    // param.normalize

    // val modOpt = opt.copy(maxIterations = opt.maxIterations / 2)

    // def optimize(opt: OptParams, data: IndexedSeq[Sentence], w: DenseVector[Double]) = {

    //   val obj = new Objective(
    //     parser,
    //     param,
    //     mkParamHolder,
    //     featureHolder,
    //     data,
    //     token,
    //     observingFeatures,
    //     numThreads)

    //   // val weights = modOpt.minimize(obj, w)
    //   val weights = opt.minimize(obj, w)
    //   obj.normalizeWith(weights)
    //   weights
    // }

    // val w1 = optimize(
    //   opt.copy(maxIterations = 50),
    //   sentences.filter(_.size <= 10),
    //   initWeight)

    // // val counts = new ExpectationCalculator(mkParamHolder, parser, token)
    // //   .collectCounts(param, learn.makePar(sentences, -1)(0 until sentences.size))
    // //   .counts
    // // val firstWeight = new LBFGS[DenseVector[Double]]().minimize(
    // //   DiffFunction.withL2Regularization(
    // //     new EMObjective(counts, featureHolder), opt.regularization),
    // //   w1)
    // // optimize(sentences, firstWeight)

    // optimize(opt, sentences, w1)

    val obj = new Objective(
      parser,
      featureHolder,
      sentences,
      observingFeatures,
      numThreads)

    val weights = opt.minimize(obj, initWeight)

    // val weights = opt.minimize(obj, firstWeight)

    obj.normalizeWith(weights) // after this, parser.param is a valid parameter
  }
}
