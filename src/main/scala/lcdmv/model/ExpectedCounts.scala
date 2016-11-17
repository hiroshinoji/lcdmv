package lcdmv.model

import lcdmv.data.Sentence

import scala.collection.GenTraversable

case class ExpectedCounts[+Param](var loss: Double, val counts: Param)

object ExpectedCounts {
  def empty[Param](emptyParam: Param) = ExpectedCounts(0.0, emptyParam)

  def collect[Param<:Parameter](
    parser: ExpectingParser[Param],
    sentenceGroups: GenTraversable[Seq[Sentence]]): ExpectedCounts[Param] = {

    def _collect(group: Seq[Sentence]): ExpectedCounts[Param] =
      parser.expectedCounts(group)

    sentenceGroups.aggregate(null: ExpectedCounts[Param])({(_countsSoFar, group) =>
      val c = _collect(group)

      val countsSoFar =
        if (_countsSoFar ne null) _countsSoFar
        else ExpectedCounts.empty(parser.mkEmptyParam())

      countsSoFar.loss += c.loss
      countsSoFar.counts += c.counts

      countsSoFar

    }, { (a, b) =>
      b.loss += a.loss
      b.counts += a.counts
      b
    })
  }
}
