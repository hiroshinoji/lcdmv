package lcdmv

import data.{CoNLLSentence, DepArcs, Dictionary, Sentence}
import model.{DMVParameter}
import java.io._

class Evaluate extends Problem {

  def run = {
    val (dict, param) = loadModel

    val parserSelector = new TestParserSelector(dict, param)
    val parser = parserSelector.mkViterbiParser()

    val reader = newCoNLLReader(ReadCondition(InputOptions.testSize, InputOptions.minLength, InputOptions.maxLength))
    val conllSentences = reader.readSentences(InputOptions.testPath)

    val sentences = conllSentences.map(stringSentenceGen(_).toTaggedSentence(dict))
    val goldArcs = conllSentences.map(_.toDepArcs)

    val predictedArcs = sentences map { s =>
      parser.parse(s).map(_._1)
    }

    val sumTokens = sentences.map(_.size).sum
    val predGoldArcs = predictedArcs.zip(goldArcs)
    val correctTokens = predGoldArcs.collect { case (Some(p), g) =>
      p.numCorrectHeads(g)
    }.sum

    val accuracy = correctTokens.toDouble / sumTokens.toDouble

    outputInCoNLL(conllSentences, predictedArcs)

    println(s"accuracy:${accuracy}:${correctTokens}/${sumTokens}")
  }

  def outputInCoNLL(sentences: IndexedSeq[CoNLLSentence], predArcs: IndexedSeq[Option[DepArcs]]) = OutputOptions.outputPath match {
    case "" =>
    case path =>
      val writer = new java.io.PrintWriter(path)
      sentences zip predArcs foreach { case (sentence, arcs) =>
        writer.write(conllString(sentence, arcs) + "\n\n")
      }
      writer.flush
      writer.close
  }
  def conllString(sentence: CoNLLSentence, arcs: Option[DepArcs]) = OutputOptions.outputFormat match {
    case OptionEnumTypes.Format.CoNLLX => sentence.toCoNLLXString(arcs)
    case _ => sentence.toPascalCoNLLString(arcs)
  }

  def loadModel: (Dictionary, DMVParameter) = {
    val path = InputOptions.loadModelPath
    println(s"Load a traied model from ${path}")
    val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(path)))
    val dict = in.readObject.asInstanceOf[Dictionary]
    val param = in.readObject.asInstanceOf[DMVParameter]
    in.close
    (dict, param)
  }
}
