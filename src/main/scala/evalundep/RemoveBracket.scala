package evalundep

import jigg.util.IOUtil
//import edu.stanford.nlp.tagger.maxent.MaxentTagger

/** ((I/PRP) am/VBP ((a/DT) scientist/NN) (./.)) => I/PRP am/VBP a/DT ...
  */
object RemoveBracket {
  def main(args: Array[String]) = {

    val deptrees = IOUtil.openStandardIterator.toSeq map { DepTree.fromBracketString(_) }
    deptrees foreach { deptree =>
      println(deptree.yields.mkString(" "))
    }
  }
}
