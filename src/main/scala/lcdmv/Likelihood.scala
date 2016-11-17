package lcdmv

import data.{Dictionary, Sentence}
import java.io._
import learn._
import model.{DMVParameter}
import OptionEnumTypes._

class Likelihood extends Evaluate {

  override def run: Unit = {

    val (dict, param) = loadModel

    val parserSelector = new TestParserSelector(dict, param)
    val parser = parserSelector.mkExpectingParser()

    val reader = newCoNLLReader(ReadCondition(InputOptions.testSize, InputOptions.minLength, InputOptions.maxLength))
    val conllSentences = reader.readSentences(InputOptions.testPath)

    val sentences = conllSentences.map(stringSentenceGen(_).toTaggedSentence(dict))

    val likelihood = sentences.map { s =>
      Math.log(parser.marginal(s))
    }.sum

    println("logprob:" + likelihood)
  }
}
