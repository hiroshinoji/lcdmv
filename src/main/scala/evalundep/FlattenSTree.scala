package evalundep

import jigg.util.IOUtil

/** FlattenSTree gold.conll pred.conll > pred.flatten.depmrg
  */
object FlattenSTree {
  def main(args: Array[String]) = {

    val gold = args(0)
    val pred = args(1)

    val goldSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(gold).toIndexedSeq)
    val predSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(pred).toIndexedSeq)

    val goldTrees = CoNLLToSTree.sentencesToSTrees(goldSentences)
    val predTrees = CoNLLToSTree.sentencesToSTrees(predSentences)

    predTrees.zip(goldTrees) foreach {
      case (Some(p), Some(g)) =>
        assert(p.spanSize == g.spanSize)
        println(Tree.flattenToAdjustToGold(p, g))
      case (None, None) =>
        println("(TOP)")
      case _ => sys.error("If either sentence is None, another should also be.")
    }
  }
}
