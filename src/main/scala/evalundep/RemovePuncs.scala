package evalundep

import jigg.util.IOUtil

object RemovePuncs {
  def main(args: Array[String]) = {
    val sentences = CoNLLSentence.readCoNLLFile(IOUtil.openStandardIterator.toIndexedSeq)
    val trees = sentences
      .map(DepTree.fromCoNLL(_))
      .map(_.toTokenizedTree)
      .map(_.toIndexedTree())
      .map(_.removeAll(_._1.isPunc))
      .map(_.map(t=>t._1.word + "/" + t._1.pos))
    trees foreach { t =>
      println(CoNLLSentence.fromIndexedStringDepTree(t.toIndexedTree()))
      println()
    }
  }
}
