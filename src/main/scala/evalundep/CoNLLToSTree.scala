package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

/** Read CoNLL, convert to S tree *with punctuations removed*
  */
object CoNLLToSTree {
  def main(args: Array[String]) = {

    val sentences = CoNLLSentence.readCoNLLFile(IOUtil.openStandardIterator.toIndexedSeq)

    sentencesToSTrees(sentences).foreach {
      case Some(tree) => println(tree)
      case None => println("(TOP)")
    }
  }

  def sentencesToSTrees(sentences: Seq[CoNLLSentence]): Seq[Option[Tree[String]]] =
    sentences
      .map(DepTree.fromCoNLL(_))
      .map(_.toTokenizedTree)
      .map(Tree.fromTokenDepTree(_))
      .map(Tree.removePuncs(_))
}
