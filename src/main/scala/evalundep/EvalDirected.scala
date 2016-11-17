package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

object EvalDirected {

  class Result(goldSentences: Seq[CoNLLSentence], predSentences: Seq[CoNLLSentence]) {
    def outputDirected() = outputHelper { case (pred, gold) => gold.countDirected(pred) }
    def outputUndirected() = outputHelper { case (pred, gold) => gold.countUndirected(pred) }
    def outputNED() = outputHelper { case (pred, gold) => gold.countNED(pred) }

    private def outputHelper(count: ((CoNLLSentence,CoNLLSentence))=>(Int, Int)) = {
      val (corrects, lengths) = predSentences.zip(goldSentences).map(count(_)).unzip
      // val complete = (0 until corrects.size).count { i => corrects(i) == lengths(i) }

      def output(maxLength: Int = -1) = {
        val idxs = (0 until lengths.size).filter { i => maxLength == -1 || lengths(i) <= maxLength }
        val token = idxs.map(corrects(_)).sum.toFloat
        val tokensum = idxs.map(lengths(_)).sum.toFloat
        val complete = idxs.count { i => corrects(i) == lengths(i) }

        println(s"token: ${token/tokensum} (${token}/${tokensum})")
        println(s"complete: ${complete/idxs.size.toFloat} (${complete}/${idxs.size})")
      }
      output()
      println("<10:")
      output(10)
      println("<15:")
      output(15)
    }
  }

  def main(args: Array[String]) = {

    val gold = args(0)
    val pred = args(1)

    def removePunc(tree: DepTree[String]) = tree.toIndexedTree().removeAll { token =>
      val slash = token._1.lastIndexOf('/')
      POSTag.isPunc(token._1.drop(slash + 1))
    }.map(_._1)

    def toNoPuncSentences(fn: String) = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(fn).toIndexedSeq)
      .map(DepTree.fromCoNLL(_))
      .map(removePunc(_))
      .map(_.toIndexedTree())
      .map(CoNLLSentence.fromIndexedStringDepTree(_))

    val goldSentences = toNoPuncSentences(gold)
    val predSentences = toNoPuncSentences(pred)

    val normalResult = new Result(goldSentences, predSentences)

    println("directed:")
    normalResult.outputDirected()
    println()

    println("undirected:")
    normalResult.outputUndirected()
    println()

    println("NED:")
    normalResult.outputNED()
    println()

    println("root:")
    val rootCount = goldSentences.zip(predSentences) map { case (g, p) => (g.root(p), g.unpuncIdxs.size) }
    println(rootCount.count(_._1).toFloat / rootCount.size.toFloat)
    println("<10:")
    val filtered = rootCount.filter(_._2 <= 10)
    println(filtered.count(_._1).toFloat / filtered.size.toFloat)
    println()

    println("flipped:")
    val (flippedGoldSentences, nopuncPredSentences) = flippedSentences(goldSentences, predSentences)

    val flippedResult = new Result(flippedGoldSentences, nopuncPredSentences)
    flippedResult.outputDirected()
  }
  // returns both sentences with punctuation removed
  def flippedSentences(goldSentences: Seq[CoNLLSentence], predSentences: Seq[CoNLLSentence]): (Seq[CoNLLSentence], Seq[CoNLLSentence]) = {
    def removePunc(tree: DepTree[(String, Int)]) = tree.removeAll { label =>
      val token = label._1
      val slash = token.lastIndexOf('/')
      POSTag.isPunc(token.drop(slash + 1))
    }

    val goldTrees = goldSentences.map(DepTree.fromCoNLLWithDummy(_)).map(removePunc(_))
    val predTrees = predSentences.map(DepTree.fromCoNLLWithDummy(_)).map(removePunc(_))

    val converter = new DepTreeConverter
    val flippedGoldTrees = goldTrees.zip(predTrees).map { case (g, p) => converter.toFlippedTree(g, p) }

    val nopuncPredSentences = predTrees.map(t => CoNLLSentence.fromIndexedStringDepTree(t))
    val nopuncGoldSentences = flippedGoldTrees.map(t => CoNLLSentence.fromIndexedStringDepTree(t))
    (nopuncGoldSentences, nopuncPredSentences)
  }
}
