package lcdmv.model

import breeze.features.FeatureVector
import breeze.linalg.{DenseVector, SparseVector}
import lcdmv.const.{Valence, Direction}
import lcdmv.data.{Sentence, SimplePoS, SimpleWord}
import org.scalatest._

class ParameterSpec extends FlatSpec with Matchers {

  "addCountsToGradient" should "calculate gradient correctly" in {

    val featureHolder = simpleFeatureHolder

    val calcedGradient = DenseVector.zeros[Double](26)

    val sentence = Array(0, 0, 1)
    val token:(Int=>Int) = sentence(_)

    val partial = new PartialCounts(3)

    partial.incrementStop(0, Direction.right, Valence.hasChild, 1.0)
    calcedGradient += SparseVector(26)((6, 1.0))
    calcedGradient -= SparseVector(26)((6, 1.0*0.5), (7, 1.0*0.5))

    // partial.incrementStop(0, Direction.left, Valence.noChild, 1.0)
    // partial.incrementProceed(1, Direction.right, Valence.noChild, 1.0)
    partial.incrementChoice(1, Direction.right, 2, 1.0) // type: (0, right, 1)
    calcedGradient += SparseVector(26)((19, 1.0))
    calcedGradient -= SparseVector(26)((18, 1.0*0.5), (19, 1.0*0.5))
    // partial.incrementChoice(1, Direction.left, 0, 1.0)
    partial.incrementRoot(1, 1.0)
    calcedGradient += SparseVector(26)((24, 1.0))
    calcedGradient -= SparseVector(26)((24, 1.0*0.5), (25, 1.0*0.5))

    val param = NaiveDMVParameter(2)
    param.map(_=>0.5)

    val expect = NaiveDMVParameter(2)
    expect.addPartial(partial, token)

    // param.gradient(featureHolder, expect) should be (calcedGradient)
  }

  def simpleFeatureHolder = {
    def gen(stopFeature: Int, proceedFeature: Int) =
      Gen(FeatureVector(stopFeature), FeatureVector(proceedFeature))

    // # types = 2 (only 0 and 1)
    val featureTable = ParamTables(
      Array(
        Array(
          gen(0, 1),     // left, noChild
          gen(2, 3),     // left, hasChild
          gen(4, 5),     // right, noChild
          gen(6, 7)),    // right, hasChild
        Array(
          gen(8, 9),     // left, noChild
          gen(10, 11),   // left, hasChild
          gen(12, 13),   // right, noChild
          gen(14, 15))), // right, hasChild
      Array(
        Array( // head = 0
          Array( // left
            FeatureVector(16),    // 0 <- 0
            FeatureVector(17)),   // 1 <- 0
          Array( // right
            FeatureVector(18),    // 0 -> 0
            FeatureVector(19))),  // 0 -> 1
        Array( // head = 1
          Array( // left
            FeatureVector(20),    // 0 <- 0
            FeatureVector(21)),   // 1 <- 0
          Array( // right
            FeatureVector(22),    // 0 -> 0
            FeatureVector(23)))), // 0 -> 1
      Array(
        FeatureVector(24),        // root = 0
        FeatureVector(25)))       // root = 1

    new DMVFeatureHolder(2, 26, featureTable)
  }

}
