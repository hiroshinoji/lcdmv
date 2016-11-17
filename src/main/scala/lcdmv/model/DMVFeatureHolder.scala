package lcdmv.model

import breeze.features.FeatureVector
import lcdmv.const.{Condition, Decision, Direction}
import scala.collection.mutable.HashMap


/** Holding multinomial parameter -> used feature indices, e.g.,
  * which features are active for choice parameter of (head, dir, argument)?
  */
@SerialVersionUID(1L)
class DMVFeatureHolder(
  val size: Int,
  val numFeatures: Int,
  val paramTables: ParamTables[FeatureVector, Array[FeatureVector]])
    extends DMVParameterLike[FeatureVector, Array[FeatureVector]] {
}


object DMVFeatureHolder {

  def apply(numTypes: Int,
    extractGen: Condition.GenCondition=>Array[String],
    extractChoice: Condition.ChoiceCondition=>Array[String],
    indexer: HashMap[String, Int]) = {

    def index:String=>Int = indexer.getOrElseUpdate(_, indexer.size)

    val builder = new TableBuilder[FeatureVector, Array[FeatureVector]] {
      def size = numTypes

      def newGen(h: Int, c: Int): Gen[FeatureVector] = {
        val stopFeatures = extractGen(Condition.GenCondition(h, c, Decision.stop)).map(index)
        val proceedFeatures = extractGen(Condition.GenCondition(h, c, Decision.proceed)).map(index)
        Gen(new FeatureVector(stopFeatures), new FeatureVector(proceedFeatures))
      }
      def newDepTable(h: Int, dir: Direction): Array[FeatureVector] = {
        (0 until numTypes).map { a =>
          val labeldFeatures = extractChoice(Condition.ChoiceCondition(h, dir, a)).map(index)
          new FeatureVector(labeldFeatures)
        }.toArray
      }
    }
    val featureTable = builder.paramTables // indexer is filled here
    new DMVFeatureHolder(numTypes, indexer.size, featureTable)
  }
}
