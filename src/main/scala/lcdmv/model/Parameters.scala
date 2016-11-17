package lcdmv.model

import breeze.linalg.DenseVector
import breeze.features.FeatureVector

import lcdmv.const.{Condition, Direction, Valence}
import lcdmv.data.{Dictionary, DepArcs}
import lcdmv.Utils

import scala.collection.mutable.HashMap
import scala.reflect.ClassTag

case class Gen[@specialized(Double, Float) GenElem](var stop: GenElem, var proceed: GenElem)

case class ParamTables[GenElem, T](
  genMap: Array[Array[Gen[GenElem]]],
  choiceMap: Array[Array[T]],
  root: T)

trait TableBuilder[GenElem, T] {
  def size: Int
  protected def newGen(h: Int, c: Int): Gen[GenElem]
  protected def newDepTable(h: Int, d: Direction): T

  def paramTables(implicit c:ClassTag[T]): ParamTables[GenElem, T] = ParamTables(
    Array.tabulate(size, 4) { (h, c) => newGen(h, c) },
    Array.tabulate(size, 2) { (h, d) => newDepTable(h, Direction(d)) },
    newDepTable(Dictionary.rootPoSID, Direction.right)
  )
}

class DenseTableBuilder(val size: Int) extends TableBuilder[Double, DepTable] {
  def newGen(h: Int, c: Int): Gen[Double] = Gen(0.0, 0.0)
  def newDepTable(h: Int, d: Direction) = new DenseDepTable(size)
}

class SparseTableBuilder(val size: Int) extends TableBuilder[Double, DepTable] {
  def newGen(h: Int, c: Int): Gen[Double] = Gen(0.0, 0.0)
  def newDepTable(h: Int, d: Direction) = new SparseDepTable(size)
}

trait DepTable extends Serializable {
  def size: Int

  def apply(key: Int): Double
  def update(i: Int, v: Double): Unit

  def activeKeys: TraversableOnce[Int]

  final def map(f: (Double=>Double)) = activeKeys foreach { i => update(i, f(apply(i))) }

  def increment(key: Int, v: Double): Unit

  def normalize(alpha: Double): Unit
  def toUninformative: Unit

  def sum: Double
}

@SerialVersionUID(1L)
class DenseDepTable(val size: Int) extends DepTable {
  val table = Array.ofDim[Double](size)
  override def apply(key: Int) = table(key)
  override def update(i: Int, v: Double) = table(i) = v

  override def activeKeys = (0 until table.size)

  override def increment(key: Int, v: Double) = table(key) += v

  override def normalize(alpha: Double) = {
    // sum match {
    //   case 0 =>
    //   case _ =>
        if (alpha == 0.0)
          (0 until table.size).foreach { i => if (table(i) <= 0.0) table(i) = 1e-20 }
        map(_+alpha)
        Utils.normalize(table)
    // }
  }

  override def toUninformative = (0 until table.size) foreach { i => table(i) = 1.0 }

  def sum = table.sum
}

/** NOTE: val t = new SpraseDepTable; t(i) += 0.5 seems dangerous
  * It expands to t(i) = t(i) + 0.5, which expands to
  * t.update(i, t(i) + 0.5), where t(i) returns not 0 but unkScore if i does not exist...
  *
  * To avoid this problem, user *should* use `increment` other than `+=` to increment a count
  */
@SerialVersionUID(1L)
class SparseDepTable(val size: Int) extends DepTable {
  val table = new HashMap[Int, Double]
  private var unkScore = 1.0 / size // (values.values.sum + unkScore) should be (1.0)

  override def apply(key: Int) = table.getOrElse(key, unkScore)
  override def update(i: Int, v: Double) = table += (i -> v)

  override def activeKeys = table.keys

  override def increment(key: Int, v: Double) = table(key) = table.getOrElse(key, 0.0) + v

  override def normalize(alpha: Double) = {
    val unkAlpha = if (alpha == 0.0) 1e-20 else alpha // even with EM, avoid 0 probability
    table.foreach { case (i, c) => table(i) =
      if (c == 0 && alpha == 0.0) 1e-20 // assign small count to zero events
      else table(i) + alpha
    }
    val denom = table.values.sum + alpha * size + unkAlpha * (size - table.size)

    assert(denom > 0.0)

    map(x=>(x+alpha)/denom)
    unkScore = unkAlpha / denom
    assert(Math.abs(table.values.sum + unkScore * (size - table.size) - 1.0) < 1e-5)
  }
  override def toUninformative = {
    table.clear
    unkScore = 1.0
  }

  def sum = table.values.sum
}

trait ParameterLike[+This<:ParameterLike[This]] {
  def clear(): Unit
  def +=(other: Parameter): This
  def normalize(): Unit
  def empty(): This
}

trait Parameter extends ParameterLike[Parameter]

trait DMVParameterLike[@specialized(Float, Double) GenElem, T] extends Serializable {
  def size: Int

  protected val paramTables: ParamTables[GenElem, T]

  def genMap = paramTables.genMap
  def choiceMap = paramTables.choiceMap
  def root = paramTables.root

  protected def condition(dir: Direction, v: Valence) = Condition.id(dir, v)

  def gen(head: Int, dir: Direction, v: Valence): Gen[GenElem] = genMap(head)(condition(dir, v))
  def gen(head: Int, cond: Int) = genMap(head)(cond)

  def choice(head: Int, dir: Direction): T = choiceMap(head)(dir.id)
  def choice(head: Int, dirID: Int) = choiceMap(head)(dirID)

  def foreachGen[U](f: (Int, Int, Gen[GenElem])=>U) =
    (0 until genMap.size) foreach { head =>
      val x = genMap(head)
      (0 until x.size) foreach { cond =>
        f(head, cond, x(cond))
      }
    }
  def foreachChoice[U](f: (Int, Int, T)=>U) =
    (0 until choiceMap.size) foreach { head =>
      val x = choiceMap(head)
      (0 until x.size) foreach { dir =>
        f(head, dir, x(dir))
      }
    }
}

trait AttachCandidate {
  def size: Int
  def activeWords(head: Int): Seq[Int]
}

trait AttachToAll extends AttachCandidate {
  def activeWords(head: Int) = (0 until size)
}

trait AttachToActiveDeps extends AttachCandidate {
  def activeWordPairs: Array[Array[Int]] // (i)(j) means i and j is co-occured in some sentence
  def activeWords(head: Int) = activeWordPairs(head)
}

@SerialVersionUID(1L)
trait DMVParameter extends DMVParameterLike[Double, DepTable] with AttachCandidate with Parameter with ParameterLike[DMVParameter] {

  def genAlpha: Double
  def choiceAlpha: Double

  def normalizeGen(gen: Gen[Double]): Unit = {
    def addSmall(v: Double) = if (v == 0.0) v + 1e-20 else v

    val stop = if (genAlpha == 0.0) addSmall(gen.stop) else gen.stop
    val proceed = if (genAlpha == 0.0) addSmall(gen.proceed) else gen.proceed
    val sum = stop + proceed + 2 * genAlpha

    gen.stop = (stop + genAlpha) / sum
    gen.proceed = (proceed + genAlpha) / sum

    // val sum = gen.stop + gen.proceed + 2 * genAlpha
    // gen.stop = gen.stop / sum
    // gen.proceed = gen.proceed / sum
  }

  def incrementStop(head: Int, dir: Direction, v: Valence, count: Double) =
    gen(head, dir, v).stop += count
  def incrementStop(head: Int, cond: Int, count: Double) =
    gen(head, cond).stop += count

  def incrementProceed(head: Int, dir: Direction, v: Valence, count: Double) =
    gen(head, dir, v).proceed += count
  def incrementProceed(head: Int, cond: Int, count: Double) =
    gen(head, cond).proceed += count

  def incrementChoice(head: Int, dir: Direction, dep: Int, count: Double) =
    choice(head, dir).increment(dep, count)
  def incrementRoot(w: Int, count: Double) = root.increment(w, count)

  def map(f: (Double=>Double)) = {
    foreachGen { case (_, _, g) => g.stop = f(g.stop); g.proceed = f(g.proceed) }
    foreachChoice { case (_, _, t) => t.map(f) }
    root.map(f)
  }

  // def foreach(f: (Double => Double)) = {
  //   foreachGen { (_, _, g) => g.stop = f(g.stop); g.proceed = f(g.proceed) }
  //   foreachChoice { (_, _, t) => t.foreachKey { i => t(i) = f(t(i)) } }
  //   root.foreachKey { i => root(i) = f(root(i)) }
  // }

  def normalize: Unit = {
    foreachGen { case (_, _, g) => normalizeGen(g) }
    foreachChoice { case (_, _, t) => t.normalize(choiceAlpha) }
    root.normalize(choiceAlpha)
  }
  def clear = map(_=>0)
  def divideBy(d: Double) = map { a =>
    if (d == 0) 0
    else a/d
  }

  def initUniform = map(_=>1.0)

  def initRandom() = {
    import util.Random
    map(_ => Random.nextDouble() + 1e-8)
    normalize
  }

  def addPartial(partial: DMVParameter, token: (Int => Int), n: Int = -1) = {
    partial.foreachGen { case (headIdx, cond, parGen) =>
      if (n == -1 || headIdx < n) {
        incrementStop(token(headIdx), cond, parGen.stop)
        incrementProceed(token(headIdx), cond, parGen.proceed)
      }
    }
    partial.foreachChoice { case (headIdx, dir, table: DepTable) =>
      if (n == -1 || headIdx < n) {
        table.activeKeys.foreach { depIdx =>
          if (n == -1 || depIdx < n) {
            incrementChoice(token(headIdx), Direction(dir), token(depIdx), table(depIdx))
          }
        }
      }
    }
    partial.root.activeKeys.foreach { rootIdx =>
      if (n == -1 || rootIdx < n)
        incrementRoot(token(rootIdx), partial.root(rootIdx))
    }
  }

  def +=(other: Parameter): this.type = other match {
    case other: DMVParameter =>
      other.foreachGen { case (head, cond, otherGen) =>
        incrementStop(head, cond, otherGen.stop)
        incrementProceed(head, cond, otherGen.proceed)
      }
      other.foreachChoice { case (head, dir, table) =>
        table.activeKeys foreach { dep =>
          incrementChoice(head, Direction(dir), dep, table(dep))
        }
      }
      other.root.activeKeys foreach { r => incrementRoot(r, other.root(r)) }
      this
    case _ => sys.error("Cannot perform adding two parameters with these two types.")
  }

  def addFeatureWeights(featureHolder: DMVFeatureHolder, weights: DenseVector[Double]) = {
    featureHolder.foreachGen { case (head, cond, parGen: Gen[FeatureVector]) =>
      incrementStop(head, cond, Math.exp(weights dot parGen.stop))
      incrementProceed(head, cond, Math.exp(weights dot parGen.proceed))
    }
    featureHolder.foreachChoice { case (head, dir, table: Array[FeatureVector]) =>
      activeWords(head) foreach { dep =>
        val features = table(dep)
        incrementChoice(head, Direction(dir), dep, Math.exp(weights dot features))
      }
      // table.zipWithIndex.foreach { case (features, dep) =>
      //   incrementChoice(head, Direction(dir), dep, Math.exp(weights dot features))
      // }
    }
    featureHolder.root.zipWithIndex.foreach { case (features, r) =>
      incrementRoot(r, Math.exp(weights dot features))
    }
  }

  def logLikelihood(counts: DMVParameter): Double = {
    var likelihood = 0.0
    counts.foreachGen { case (head, cond, count) =>
      likelihood += count.stop * Math.log(gen(head, cond).stop)
      likelihood += count.proceed * Math.log(gen(head, cond).proceed)
    }
    counts.foreachChoice { case (head, dir, count) =>
      likelihood += (0 until counts.size).map(a => count(a) * Math.log(choice(head, dir)(a))).sum
    }
    likelihood += (0 until root.size).map(r => counts.root(r) * Math.log(root(r))).sum
    likelihood
  }

  def gradient(featureHolder: DMVFeatureHolder, counts: DMVParameter): DenseVector[Double] = {
    val grad = DenseVector.zeros[Double](featureHolder.numFeatures)
    addCountsToGradient(featureHolder, counts, grad)
    grad
  }

  def addCountsToGradient(
    featureHolder: DMVFeatureHolder, counts: DMVParameter, grad: DenseVector[Double]) = {

    counts.foreachGen { case (head, cond, count) =>
      val featureGen = featureHolder.gen(head, cond)
      val totalCount = count.stop + count.proceed

      val stopProb = gen(head, cond).stop
      featureGen.stop.data foreach { k =>
        grad(k) += count.stop
        grad(k) += -totalCount * stopProb
      }
      val proceedProb = gen(head, cond).proceed
      featureGen.proceed.data foreach { k =>
        grad(k) += count.proceed
        grad(k) += -totalCount * proceedProb
      }
    }
    counts.foreachChoice { case (head, dir, count: DepTable) =>
      val featureChoice = featureHolder.choice(head, dir)
      val choiceProb = choice(head, dir)

      val totalCount = activeWords(head).map(count(_)).sum // count.sum

      activeWords(head) foreach { dep =>
      // (0 until counts.size) foreach { dep =>
        featureChoice(dep).data foreach { k =>
          grad(k) += count(dep)
          grad(k) += -totalCount * choiceProb(dep)
        }
      }
    }
    val totalRootCount = counts.root.sum
    (0 until root.size) foreach { r =>
      featureHolder.root(r).data foreach { k =>
        grad(k) += counts.root(r)
        grad(k) += -totalRootCount * root(r)
      }
    }
  }

  def addCountsToGradient(
    featureHolder: DMVFeatureHolder,
    partial: PartialCounts,
    gradient: DenseVector[Double],
    token: (Int=>Int)) = {

    partial.foreachGen { case (headIdx, cond, parCount) =>
      val head = token(headIdx)

      val featureGen = featureHolder.gen(head, cond)
      val stopFeatures = featureGen.stop
      val proceedFeatures = featureGen.proceed

      val stopProb = gen(head, cond).stop
      val proceedProb = gen(head, cond).proceed

      stopFeatures.data foreach { k =>
        gradient(k) += (parCount.stop * (1.0 - stopProb))
        gradient(k) += (parCount.proceed * -stopProb)
      }
      proceedFeatures.data foreach { k =>
        gradient(k) += (parCount.stop * -proceedProb)
        gradient(k) += (parCount.proceed * (1.0 - proceedProb))
      }
    }
    partial.foreachChoice { case (headIdx, dir, parCount: DepTable) =>
      val head = token(headIdx)
      val featureChoice = featureHolder.choice(head, dir)
      val choiceProb = choice(head, dir)

      (0 until parCount.size) foreach { depIdx =>
        val dep = token(depIdx)
        (0 until parCount.size) foreach { depCandIdx =>
          val depCand = token(depCandIdx)

          if (depIdx == depCandIdx) {
            featureChoice(depCand).data foreach { k =>
              gradient(k) += parCount(depIdx) * (1.0 - choiceProb(dep))
            }
          } else {
            featureChoice(depCand).data foreach { k =>
              gradient(k) += parCount(depIdx) * -choiceProb(depCand)
            }
          }
        }
      }
    }
    (0 until partial.root.size) foreach { rootIdx =>
      val r = token(rootIdx)
      (0 until partial.root.size) foreach { rootCandIdx =>
        val rCand = token(rootCandIdx)

        if (rootIdx == rootCandIdx) {
          featureHolder.root(rCand).data foreach { k =>
            gradient(k) += (partial.root(rootIdx) * (1.0 - root(r)))
          }
        } else {
          featureHolder.root(rCand).data foreach { k =>
            gradient(k) += (partial.root(rootIdx) * -root(rCand))
          }
        }
      }
    }
  }

  def finalizeHarmonic(uniformRoot: Boolean = true) = {
    if (uniformRoot) { // discard accumulated root scores
      root.map(_=>1.0)
    }
    foreachGen { case(_, _, g) => g.stop += 1.0; g.proceed += 1.0 }
  }

  def finalizeHarmonicNoah(norm: DMVParameter) = {
    def firstChildNoah(): Double = {
      var e = 1.0
      foreachGen { case (h, cond, gen) =>
        def maybeUpdate(num: Double, denom: Double) = {
          val ratio = -1.0 * num / denom
          if (denom < 0 && e > ratio) e = ratio
        }
        maybeUpdate(gen.stop, norm.gen(h, cond).stop)
        maybeUpdate(gen.proceed, norm.gen(h, cond).proceed)
      }
      e
    }
    map(_ + Math.pow(10, -1))

    val prFirstKid = 0.9 * firstChildNoah()

    norm.foreachGen { case (h, cond, gen) =>
      gen.stop *= prFirstKid
      gen.proceed *= prFirstKid
    }
    foreachGen { case (h, cond, gen) =>
      gen.stop += norm.gen(h, cond).stop
      gen.proceed += norm.gen(h, cond).proceed
    }
  }

  def writeTo(out: java.io.Writer, token: (Int=>String), conv:Double=>Double = (x=>x)) = {
    foreachChoice { case (head, dir, choice) =>
      (0 until choice.size) foreach { dep =>
        out.write(s"D\t${Direction(dir)}\t${token(head)}\t${token(dep)}\t${conv(choice(dep))}\n")
      }
    }
    (0 until root.size) foreach { r =>
      out.write(s"D\troot\t${token(r)}\t${conv(root(r))}\n")
    }

    foreachGen { case (head, cond, gen) =>
      out.write(s"C\t${Condition.dir(cond)}\t${Condition.valence(cond)}\t${token(head)}\tstop\t${conv(gen.stop)}\n")
      out.write(s"C\t${Condition.dir(cond)}\t${Condition.valence(cond)}\t${token(head)}\tnonstop\t${conv(gen.proceed)}\n")
    }
  }

  // def empty(): DMVParameter
}

trait DenseDMVParameter extends DMVParameter {
  override val paramTables = new DenseTableBuilder(size).paramTables
}

@SerialVersionUID(1L)
case class NaiveDMVParameter(size: Int, genAlpha: Double = 0.0, choiceAlpha: Double = 0.0)
    extends DenseDMVParameter with AttachToAll with ParameterLike[NaiveDMVParameter] {

  def empty() = this.copy()
}

@SerialVersionUID(1L)
case class FilteredNaiveDMVParameter(val activeWordPairs: Array[Array[Int]], genAlpha: Double = 0.0, choiceAlpha: Double = 0.0)
    extends DenseDMVParameter with AttachToActiveDeps with ParameterLike[FilteredNaiveDMVParameter] {
  def size = activeWordPairs.size
  def empty(): FilteredNaiveDMVParameter = this.copy()
}


@SerialVersionUID(1L)
case class SparseDMVParameter(size: Int, genAlpha: Double = 0.0, choiceAlpha: Double = 0.0)
    extends DMVParameter with AttachToAll with ParameterLike[SparseDMVParameter] {
  override val paramTables = new SparseTableBuilder(size).paramTables
  def empty(): SparseDMVParameter = this.copy()
}

// the index of this class is token index of a sentence
class PartialCounts(override val size: Int) extends DenseDMVParameter with AttachToAll with ParameterLike[PartialCounts] {

  override val paramTables = new DenseTableBuilder(size).paramTables

  val genAlpha = 0.0
  val choiceAlpha = 0.0

  def addHarmonicCounts(cAttach: Double = 1.0) = {
    val cStop = 1.0
    val cProceed = 1.0
    val stopUniformity = 1.0

    (0 until size) foreach { h =>
      (0 until h) foreach { d =>
        incrementChoice(h, Direction.left, d, 1.0 / (h - d) + cAttach)
      }
      (h + 1 until size) foreach { d =>
        incrementChoice(h, Direction.right, d, 1.0 / (d - h) + cAttach)
      }
      if (h == 0) incrementStop(h, Direction.left, Valence.noChild, cStop)
      else incrementProceed(h, Direction.left, Valence.noChild, cProceed)
      if (h == 1) incrementStop(h, Direction.left, Valence.hasChild, cStop)
      else incrementProceed(h, Direction.left, Valence.hasChild, cProceed)
      // else if (h != 0) incrementProceed(h, Left, Valence.hasChild, cProceed)

      if (h == size - 1) incrementStop(h, Direction.right, Valence.noChild, cStop)
      else incrementProceed(h, Direction.right, Valence.noChild, cProceed)
      if (h == size - 2) incrementStop(h, Direction.right, Valence.hasChild, cStop)
      else incrementProceed(h, Direction.right, Valence.hasChild, cProceed)

      incrementRoot(h, cAttach)
    }
  }

  /** Noah Smith's version of harmonic initializer which is lightly noted on Smith and Eisner (2006).
    */
  def addHarmonicChoiceCountsNoah() = {
    (0 until size) foreach { incrementRoot(_, 1.0 / size) }

    // choice counts
    (0 until size) foreach { chi =>
      val sum = (0 until size).map {
        case `chi` => 0
        case par => 1.0 / Math.abs(par - chi)
      }.sum

      val scale = ((size - 1).toDouble / size.toDouble) * (1.0 / sum)

      (0 until size) foreach {
        case `chi` =>
        case par =>
          val update = scale * (1.0 / Math.abs(chi - par))
          val dir = if (par > chi) Direction.left else Direction.right
          incrementChoice(par, dir, chi, update)
      }
    }
  }
  def addHarmonicStopCountsNoah() = {
    foreachChoice {
      case (par, dir, table) =>
        table.sum match {
          case sum if sum > 0 =>
            incrementProceed(par, Direction(dir), Valence.hasChild, sum)
            incrementStop(par, Direction(dir), Valence.noChild, 1.0)
          case _ =>
            incrementStop(par, Direction(dir), Valence.noChild, 1.0)
        }
      case _ =>
    }
  }
  def addHarmonicStopNormNoah(partial: PartialCounts) = {
    partial.foreachChoice {
      case (par, dir, table) if table.sum > 0 =>
        incrementProceed(par, Direction(dir), Valence.noChild, 1.0)
        incrementProceed(par, Direction(dir), Valence.hasChild, -1.0)
        incrementStop(par, Direction(dir), Valence.noChild, -1.0)
        incrementStop(par, Direction(dir), Valence.hasChild, 1.0)
      case _ =>
    }
  }

  def addSupervisedCounts(arcs: DepArcs) = {
    def addSubtreeCounts(root: Int): Unit = {
      val leftDeps = arcs.leftDependents(root)
      val rightDeps = arcs.rightDependents(root)

      leftDeps.foreach { d =>
        incrementChoice(root, Direction.left, d, 1.0)
        addSubtreeCounts(d)
      }
      rightDeps.foreach { d =>
        incrementChoice(root, Direction.right, d, 1.0)
        addSubtreeCounts(d)
      }

      leftDeps match {
        case Seq(first, remains@_*) =>
          incrementProceed(root, Direction.left, Valence.noChild, 1.0)
          remains foreach { _ =>
            incrementProceed(root, Direction.left, Valence.hasChild, 1.0)
          }
          incrementStop(root, Direction.left, Valence.hasChild, 1.0)
        case _ =>
          incrementStop(root, Direction.left, Valence.noChild, 1.0)
      }
      rightDeps match {
        case Seq(first, remains@_*) =>
          incrementProceed(root, Direction.right, Valence.noChild, 1.0)
          remains foreach { _ =>
            incrementProceed(root, Direction.right, Valence.hasChild, 1.0)
          }
          incrementStop(root, Direction.right, Valence.hasChild, 1.0)
        case _ =>
          incrementStop(root, Direction.right, Valence.noChild, 1.0)
      }
    }
    arcs.nonDummyRoots foreach { r =>
      addSubtreeCounts(r)
      incrementRoot(r, 1.0)
    }
  }

  def empty() = new PartialCounts(size)
}
