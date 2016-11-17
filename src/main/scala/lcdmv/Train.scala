package lcdmv

import data.{Dictionary, Sentence}
import java.io._
import learn._
import model.{DMVParameter}
import OptionEnumTypes._

class Train extends Problem {
  override def run: Unit = {

    val (sentences, dict) = mkData()

    val parserSelector = new TrainParserSelector(dict, sentences)

    val param = parserSelector.param

    init(param, getCounts(parserSelector, sentences))

    val trainer = new EMTrainer(
      parserSelector.mkExpectingParser(),
      sentences,
      TrainingOptions.numThreads)

    trainer.run(TrainingOptions.numIters)

    saveModel(dict, param)
  }

  def mkData() = {
    val reader = newReader()
    val allTrainSentences = reader match {
      case reader: CoNLLReader =>
        reader.readSentences(InputOptions.trainPath).map(stringSentenceGen(_))
      case reader: TwoLineReader =>
        reader.readSentences(InputOptions.trainPath)
    }
    val trainCond = ReadCondition(InputOptions.trainSize, InputOptions.minLength, InputOptions.maxLength)
    val activeTrainSentences = trainCond.reduce(allTrainSentences)

    def allTestSentences = reader match {
      case reader: CoNLLReader =>
        reader.readSentences(InputOptions.testPath).map(stringSentenceGen(_))
      case reader: TwoLineReader =>
        reader.readSentences(InputOptions.testPath)
    }

    val stringSentences = InputOptions.howToRead match {
      case HowToReadAtTrain.allTrain =>
        allTrainSentences
      case HowToReadAtTrain.activeTrain =>
        activeTrainSentences
      case HowToReadAtTrain.allTrainAndTest =>
        allTrainSentences ++ allTestSentences
      case HowToReadAtTrain.activeTrainAndTest =>
        activeTrainSentences ++ allTestSentences // activeTestSentences
    }
    val dict = newDictionary(stringSentences)
    val trainSentences = activeTrainSentences.map(_.toTaggedSentence(dict))

    println(s"training size: ${trainSentences.size}")
    // (trainSentences.sortBy(_.size), dict)
    (trainSentences, dict)
  }

  def init(param: DMVParameter, counts: DMVParameter) = {
    param.clear
    param += counts
    if (TrainingOptions.doNormalize) param.normalize
  }

  def getCounts(
    selector: ParserSelector,
    sentences: IndexedSeq[Sentence]): DMVParameter = {

    val counter = new CountCollector(
      sentences, tokenFun(_), selector._mkEmptyParam, _mkPartial)

    val counts = TrainingOptions.initializer match {
      case Initializer.harmonic =>
        counter.collectEMHarmonic(selector.mkHarmonicParser())
      case Initializer.uniform => counter.collectUniform()
      case Initializer.random => counter.collectRandom()
      case Initializer.supervise => counter.collectSupervise(goldArcs())
    }
    counts
  }

  def goldArcs() = newCoNLLReader(
    ReadCondition(
      InputOptions.trainSize,
      InputOptions.minLength,
      InputOptions.maxLength)).readSentences(InputOptions.trainPath).map(_.toDepArcs)

  def saveModel(dict: Dictionary, param: DMVParameter): Unit = {
    val path = OutputOptions.saveModelPath
    println(s"Save traied model to ${path}")
    val os = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(path)))
    os.writeObject(dict)
    os.writeObject(param)
    os.close

    saveFeature(dict, param)
  }

  def saveFeature(dict: Dictionary, param: DMVParameter): Unit = OutputOptions.parserFeaturePath match {
    case "" =>
    case featurePath =>
      val writer = new java.io.PrintWriter(featurePath)
      param.writeTo(writer, tokenId2Str(dict))
      writer.flush
      writer.close
  }
}
