package evalundep

import jigg.util.IOUtil

object TheDownfell {
  def main(args: Array[String]) = {

    // val gold = "(((The/X) Down/X) fell/X ((22.6/X) %/X) (on/X (Black/X (Monday/X))))"
    // val dmv = "((The/X (Down/X)) fell/X ((22.6/X (%/X)) on/X (Black/X (Monday/X))))"
    // val feature = "(((The/X) Down/X) fell/X (((22.6/X) %/X) on/X ((Black/X) Monday/X)))"

    // val gold = "((At/X ((this/X) point/X)) (the/X (dow/X)) was/X)"
    // val dmv = "(((At/X (this/X (point/X))) the/X (dow/X)) was/X)"
    // val feature = "(((At/X (this/X (point/X))) the/X (dow/X)) was/X)"

    // val gold = "(is/is (considering/cons ((a/a) (settlement/settle) proposal/prop (made/made))))"
    // val dmv = "(is/is (considering/cons (a/a ((settlement/settle) proposal/prop))) (made/made))"
    // val feature = "(is/is (considering/cons (a/a ((settlement/settle) proposal/prop))) (made/made))"

    val gold = "(is/is ((a/a) (holding/hold) company/comp (controlled/cont)))"
    val dmv = "(is/is (a/a ((holding/hold) company/comp)) (controlled/cont))"
    val feature = "(is/is (a/a ((holding/hold) company/comp)) (controlled/cont))"

    val goldDepTree = DepTree.fromBracketString(gold)
    val goldSentence = CoNLLSentence.fromIndexedStringDepTree(goldDepTree.toIndexedTree())

    val dmvDepTree = DepTree.fromBracketString(dmv)
    val dmvSentence = CoNLLSentence.fromIndexedStringDepTree(dmvDepTree.toIndexedTree())

    val featureDepTree = DepTree.fromBracketString(feature)
    val featureSentence = CoNLLSentence.fromIndexedStringDepTree(featureDepTree.toIndexedTree())

    def calc(predSentence: CoNLLSentence) = {
      def output(cnt:(Int, Int)) = s"${cnt._1} / ${cnt._2} (${cnt._1.toFloat/cnt._2.toFloat})"

      println("directed: " + output(goldSentence.countDirected(predSentence)))
      println("undirected: " + output(goldSentence.countUndirected(predSentence)))
      println("NED: " + output(goldSentence.countNED(predSentence)))
    }

    println("dmv:")
    calc(dmvSentence)
    println("feature:")
    calc(featureSentence)
  }
}
