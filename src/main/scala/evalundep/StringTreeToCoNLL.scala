package evalundep

import jigg.util.IOUtil

object StringTreeToCoNLL {
  def main(args: Array[String]) = {
    val deptrees = IOUtil.openStandardIterator.toSeq map { DepTree.fromBracketString(_).toIndexedTree() }
    deptrees foreach { deptree =>
      println(CoNLLSentence.fromIndexedStringDepTree(deptree))
      println()
    }
  }
}
