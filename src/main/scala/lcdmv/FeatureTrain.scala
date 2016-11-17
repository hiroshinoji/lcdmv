package lcdmv

import breeze.linalg.DenseVector
import breeze.optimize.FirstOrderMinimizer.OptParams
import data.{Dictionary, PoS, Sentence}
import java.io._
import learn._
import model.{DMVFeatureHolder, DMVParameter, ExpectingParser}
import OptionEnumTypes._
import scala.collection.mutable.HashMap

class FeatureTrain extends Train {

  override def run: Unit = {

    val (sentences, dict) = mkData()

    val parserSelector = new TrainParserSelector(dict, sentences)

    val indexer = new HashMap[String, Int]
    println("Extracting features...")
    val featureHolder = makeFeatures(indexer, dict)  // indexer is filled with this method
    println("done. # features: " + featureHolder.numFeatures)

    val idxToF: Seq[String] = indexer.toSeq.sortWith(_._2 < _._2).map(_._1)

    val weight = initParamAndWeight(parserSelector, featureHolder, sentences)

    val observingFeatures = Seq(
      // "choice_noun_a_verb_h",
      // "choice_noun_a_verb_h_dir=left",
      // "choice_noun_a_verb_h_dir=right"//,
      // "choice_a=DT_verb_h",
      // "choice_a=DT_verb_h_dir=left",
      // "choice_a=DT_verb_h_dir=right"
    ).map(f => (f, indexer(f)))

    val trainer = new DirectGradientTrainer(
      parserSelector.mkExpectingParser(),
      featureHolder,
      sentences,
      TrainingOptions.numThreads)

    trainer.run(optParams, weight, observingFeatures)

    saveModel(dict, parserSelector.param)
  }

  def makeFeatures(indexer: HashMap[String, Int], dict: Dictionary): DMVFeatureHolder = {
    val pos:(Int=>PoS) = posID => dict.pos(posID)

    val genExtractors = new FeatureExtractors(Array(new PainlessGenExtractor(pos)))
    val choiceExtractors = new FeatureExtractors(Array(new PainlessChoiceExtractor(pos)))

    DMVFeatureHolder(vocab(dict), genExtractors.extract(_), choiceExtractors.extract(_), indexer)
  }

  def initParamAndWeight(
    selector: ParserSelector,
    featureHolder: DMVFeatureHolder,
    sentences: IndexedSeq[Sentence]): DenseVector[Double] = {

    val counts = getCounts(selector, sentences)

    val initializer = new FeatureInitializer(counts, featureHolder)

    TrainingOptions.initializer match {
      case Initializer.uniform =>
        initializer.unoptimizedParamAndWeight(selector.param)
      case _ =>
        initializer.optimizedParamAndWeight(selector.param, sentences, ReguralizeOptions.regularization)
    }
  }

  def optParams = {
    import ReguralizeOptions._
    println(s"regularization: ${regularization}")
    println(s"stochastic: ${useStochastic}")
    OptParams(batchSize, regularization, alpha, TrainingOptions.numIters, useL1, tolerance, useStochastic)
  }
}
