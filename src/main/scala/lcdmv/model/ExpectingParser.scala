package lcdmv.model

import lcdmv.data.{Sentence}

import scala.collection.GenTraversable

trait ExpectingParser[Param] extends Parser[Param] {

  protected def mkChart(
    sentence: Sentence, partial: Param, inside: Table, outside: Table): Chart

  /** This does not cache the table so is not very efficient.
    */
  def mkChart(sentence: Sentence): Chart = {
    val partial: Param = mkPartial(sentence.size)
    val inside = mkTable(sentence.size)
    val outside = mkTable(sentence.size)

    mkChart(sentence, partial, inside, outside)
  }

  def marginal(sentence: Sentence): Double = {
    val chart = mkChart(sentence)
    chart.marginal()
  }

  // def incrementCounts(sentence: Sentence): Param = {
  //   val chart = mkChart(sentence)
  //   chart.incrementCounts()
  //   chart.partial
  // }

  def expectation(sentence: Sentence): ExpectedCounts[Param] = {
    val chart = mkChart(sentence)
    val marginal = expectationHelper(chart)
    ExpectedCounts(marginal, chart.partial)
  }

  def expectedCounts(sentences: GenTraversable[Sentence]): ExpectedCounts[Param] = {
    val maxN = sentences.maxBy(_.size).size

    val insideTable = mkTable(maxN)
    val outsideTable = mkTable(maxN)

    val partial = mkPartial(maxN)

    sentences.foldLeft(null: ExpectedCounts[Param]) {(_countsSoFar, sentence) =>
      val paramHolder = mkParamHolder(sentence, partial)
      paramHolder.clearPartial(sentence.size)

      insideTable.clearCounts(sentence.size)
      outsideTable.clearCounts(sentence.size)

      val chart = mkChart(sentence, partial, insideTable, outsideTable)
      val marginal = expectationHelper(chart)

      val countsSoFar =
        if (_countsSoFar ne null) _countsSoFar
        else ExpectedCounts.empty(mkEmptyParam())

      if (marginal > 0) {
        countsSoFar.loss += Math.log(marginal)
        paramHolder.addPartialToCounts(countsSoFar.counts)
      }
      countsSoFar
    }
  }

  private def expectationHelper(chart: Chart): Double = {
    val marginal = chart.incrementCounts()
    chart.marginal()
  }
}
