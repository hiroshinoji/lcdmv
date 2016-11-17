package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

object TransferNoisyTags {
  def main(args: Array[String]) = {

    val orig = args(0) // original file which we want to change the tag column
    val pred = args(1) // tagged file that we want to transfer the prediction into the arg(1) file

    val goldSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(orig).toIndexedSeq)
    val predSentences = CoNLLSentence.readCoNLLFile(IOUtil.openIterator(pred).toIndexedSeq)

    predSentences.zip(goldSentences) foreach { case (p, g) =>
      println(g.copy(cpos = g.cpos, pos = p.pos))
      println()
    }
  }
}
