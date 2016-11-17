package evalundep

import jigg.util.IOUtil

/** Read two trees, output flipped gold trees (in stdout)
  */
object FlipTrees {
  def main(args: Array[String]) = {

    val gold = args(0)
    val pred = args(1)

    val goldSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(gold).toIndexedSeq)
    val predSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(pred).toIndexedSeq)

    val goldTrees = goldSentences.map(DepTree.fromCoNLLWithDummy(_))
    val predTrees = predSentences.map(DepTree.fromCoNLLWithDummy(_))

    val converter = new DepTreeConverter

    goldTrees.zip(predTrees).foreach { case (g, p) =>
      val flippedGold = converter.toFlippedTree(g, p)
      println(CoNLLSentence.fromIndexedStringDepTree(flippedGold))
      println()
    }

  }
}
