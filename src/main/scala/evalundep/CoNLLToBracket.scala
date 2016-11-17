package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

/** Read CoNLL, convert to Huang's style bracket
  */
object CoNLLToBracket {
  def main(args: Array[String]) = {

    val sentences = CoNLLSentence.readCoNLLFile(IOUtil.openStandardIterator.toIndexedSeq)

    sentences.map(DepTree.fromCoNLL(_)) foreach { deptree =>
      println(deptree)
    }
  }
}
