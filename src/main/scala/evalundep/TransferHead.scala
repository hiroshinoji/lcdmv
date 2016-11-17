package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

/** This is the opposite of TransferNoisyTags
  *
  * Instead of modifying the POS label, this change the head column
  */
object TransferHead {
  def main(args: Array[String]) = {

    val orig = args(0) // original file which we want to change the head column
    val pred = args(1) // tagged file that we want to transfer the head info into the arg(0) file

    val goldSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(orig).toIndexedSeq)
    val predSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(pred).toIndexedSeq)

    predSentences.zip(goldSentences) foreach { case (p, g) =>
      println(g.copy(head = p.head))
      println()
    }
  }
}
