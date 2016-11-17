package evalundep

import jigg.util.IOUtil

object InjectPPAttachmentErrors {

  def main(args: Array[String]) = {

    val sentences = CoNLLSentence.readCoNLLFile(IOUtil.openStandardIterator.toIndexedSeq)

    val trees = sentences
      .map(DepTree.fromCoNLL(_))
      .map(_.toTokenizedTree)
      .map(_.toIndexedTree())
      .map(_.removeAll(_._1.isPunc))
      .map(_.map(_._1))

    val converter = new DepTreeConverter

    trees foreach { t =>
      val injected = converter.injectNoiseOfPPAttachment(t)

      val indexed = injected.map { t => t.word + "/" + t.pos }.toIndexedTree()
      val sent = CoNLLSentence.fromIndexedStringDepTree(indexed)

      if (t.spanSize != injected.spanSize) {
        println(sent)
        println(CoNLLSentence.fromIndexedStringDepTree(t.map(t=>t.word+"/"+t.pos).toIndexedTree()))
      }
      assert(t.spanSize == injected.spanSize)

      println(sent)
      println()
    }
  }
}
