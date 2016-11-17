package lcdmv

import data.{Dictionary, Sentence}
import java.io._
import learn._
import model.{DMVParameter}
import OptionEnumTypes._

class CalcCoverage extends Train {

  override def run: Unit = {

    val (sentences, dict) = mkData()

    val stat = calcLengthToNumTrees(dict, sentences)

    val writer = OutputOptions.outputPath match {
      case "" => new java.io.PrintWriter(System.out)
      case path => new java.io.PrintWriter(path)
    }
    stat.toSeq.sortBy(_._1).foreach { x => writer.write(x._1 + " " + x._2 + "\n") }
    writer.flush()
    writer.close()
  }

  def calcLengthToNumTrees(dict: Dictionary, sentences: IndexedSeq[Sentence]): Map[Int, Double] = {
    val parserSelector = new TrainParserSelector(dict, sentences)
    val parser = parserSelector.mkExpectingParser()
    val param = parserSelector.param

    param.initUniform // with this, chart's marginal gets equal to the number of allowable trees

    sentences.map(s => (s.size, parser.marginal(s)))
      .groupBy(_._1)
      .map { case (length, values) => (length, values.map(_._2).sum / values.size) }
      .toMap
  }
}
