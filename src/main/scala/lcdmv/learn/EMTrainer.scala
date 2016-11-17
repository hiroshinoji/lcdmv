package lcdmv.learn

import lcdmv.data.{Sentence}
import lcdmv.model.{Parameter, ExpectedCounts, ExpectingParser, ParamHolder}

import collection.GenTraversable

class EMTrainer[Param<:Parameter](
  val parser: ExpectingParser[Param],
  val sentenceGroups: GenTraversable[Seq[Sentence]]) {

  def this(parser: ExpectingParser[Param],
    data: IndexedSeq[Sentence],
    // token: Sentence=>Int=>Int,
    numThreads: Int = -1) = this(parser,
      // learn.makePar(, numThreads)(0 until 20))
      // Seq(data))
      learn.parGroups(data, 20, numThreads))

  def run(numIters: Int) = {
    (0 until numIters) foreach { i =>
      val counts = eStep()
      println(s"logprob:${i}:${counts.loss}")
      mStep(counts.counts)
    }
  }

  // sum expected counts
  protected def eStep(): ExpectedCounts[Param] =
    ExpectedCounts.collect(parser, sentenceGroups)

  // normalize
  protected def mStep(counts: Param) = {
    parser.param.clear
    parser.param += counts
    parser.param.normalize
  }
}
