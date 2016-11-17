// package evalundep

// import jigg.util.IOUtil
// import edu.stanford.nlp.tagger.maxent.MaxentTagger

// object ChangePoSByStanford {
//   def main(args: Array[String]) = {
//     val conllSentences = CoNLLSentence.readCoNLLFile(IOUtil.openStandardIterator.toIndexedSeq)

//     val tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger")

//     val sentence = conllSentences(0).word.mkString(" ")
//     val tagged = tagger.tagString(sentence)
//     println(tagged)
//   }
// }
