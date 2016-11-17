package evalundep

import jigg.util.IOUtil
import java.io.File

/** Receive two conll sentences (may include punctuations) and compare scores with EVALB and NED
  *
  * Which sentence is regarded as entirely correct with only one metric?
  */
object CompareEvalbAndNED {

  type TS = Tree[String]

  val evalbHome = "/Users/noji/tmp/EVALB"
  val evalbCommand = s"${evalbHome}/evalb -p ${evalbHome}/sample/sample.prm"

  val maxSentenceLength = 10

  def main(args: Array[String]) = {
    val gold = args(0)
    val pred = args(1)

    val goldSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(gold).toIndexedSeq)
    val predSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(pred).toIndexedSeq)

    val goldSTrees = CoNLLToSTree.sentencesToSTrees(goldSentences)
    val predSTrees = CoNLLToSTree.sentencesToSTrees(predSentences)

    val evalbResult = evalbResultFromTrees(goldSTrees, predSTrees)

    println(evalbResult.totalStat)
    println(evalbResult.f1)
    println(evalbResult.complete)

  }

  def evalbResultFromTrees(goldSTrees: Seq[Option[TS]], predSTrees: Seq[Option[TS]]) =
    IOUtil.withTemp("gold", ".depmrg") { goldFile =>
      IOUtil.withTemp("pred", "depmrg") { predFile =>
        writeSTree(goldFile, goldSTrees)
        writeSTree(predFile, predSTrees)
        EvalbResult.fromCommand(evalbCommand + " %s %s".format(goldFile.getPath, predFile.getPath))
      }
    }

  def writeSTree(file: File, trees: Seq[Option[TS]]) = {
    val stream = IOUtil.openOut(file)
    trees foreach {
      case Some(tree) => stream.write(tree + "\n")
      case None => stream.write("(TOP)\n")
    }
    stream.flush
    stream.close
  }

  case class DepStat(numDA: Int, numUA: Int, numNED: Int, numTokens: Int)

  // case class DepStats(stats: Seq[DepStat]) = {
  //   def
  // }

  def calcDepStats(goldSentences: Seq[CoNLLSentence], predSentences: Seq[CoNLLSentence]): Seq[DepStat] =
    goldSentences.zip(predSentences).map { case (g, p) =>
      DepStat(g.countDirected(p)._1, g.countUndirected(p)._1, g.countNED(p)._1, g.unpuncIdxs.size)
    }
}
