package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

/** EvalFScore gold.conll pred.conll
  */
object EvalFScore {
  def main(args: Array[String]) = {

    val gold = args(0)
    val pred = args(1)

    val goldSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(gold).toIndexedSeq)
    val predSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(pred).toIndexedSeq)

    val goldTrees = goldSentences.map(DepTree.fromCoNLL(_)).map(_.toTokenizedTree).map(Tree.fromTokenDepTree(_))
    val predTrees = predSentences.map(DepTree.fromCoNLL(_)).map(_.toTokenizedTree).map(Tree.fromTokenDepTree(_))


  }
}
