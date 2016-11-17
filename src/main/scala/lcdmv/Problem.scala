package lcdmv

import data._
import OptionEnumTypes._
import model._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

trait Problem {
  def run: Unit

  lazy val stringSentenceGen: (CoNLLSentence=>StringSentence) =
    ParserOptions.posdef match {
      case Posdef.pos => _.toStringSentence
      case Posdef.cpos => _.toStringSentenceWithCPos
      case Posdef.upos => _.toStringSentenceWithUPos
    }

  def newReader(cond: ReadCondition = ReadCondition(0, 0, 0)) =
    InputOptions.fileFormat match {
      case Format.CoNLLX => new CoNLLXReader(cond)
      case Format.PascalCoNLL => new PascalCoNLLReader(cond)
      case Format.twoline => new TwoLineReader(cond)
    }

  def newCoNLLReader(cond: ReadCondition = ReadCondition(0, 0, 0)) =
    InputOptions.fileFormat match {
      case Format.CoNLLX => new CoNLLXReader(cond)
      case Format.PascalCoNLL => new PascalCoNLLReader(cond)
      case _ => sys.error("Please specify conll format.")
    }

  def newDictionary(conllSentences: Seq[StringSentence]): Dictionary = {
    val wordManager = new SimpleWordManager(unkConverter)
    val posManager = new SimplePoSManager
    val dict = new SimpleDictionary(wordManager, posManager, tokenModifiers)

    val posToUpos = conllSentences.flatMap { s => s.pos.zip(s.upos) }.toMap
    dict.registPosMagging(posToUpos)

    val posSet = conllSentences.flatMap(_.pos).toSet
    posSet foreach { pos => dict.posOrCreate(pos) }

    val typeCounts = conllSentences.flatMap(_.word).groupBy(w=>w).map {
      case (typ, tokens) => (typ, tokens.size)
    }
    if (InputOptions.unkThreshold > 0) {
      val activeWords = typeCounts.filter { case (t, c) => c > InputOptions.unkThreshold }.map(_._1)
      activeWords.foreach { w => dict.wordOrCreate(w) }
      dict.registUnknownWordsWith(typeCounts.map(_._1).toSeq)
    } else {
      conllSentences.foreach { _.word foreach { w => dict.wordOrCreate(w) } }
    }
    println("dictionary entry: " + dict.wordManager.size)

    ParserOptions.inputToken match {
      case InputToken.surface =>
        println("used token: word surface")
      case InputToken.pos =>
        println("used token: pos")
    }
    println("# types: " + vocab(dict))

    dict
  }

  private def unkConverter: data.TokenModifier = InputOptions.unkConverter match {
    case UnkConverter.simple => new SimpleUnkConverter
    case UnkConverter.surfaceFeature => new SurfaceFeatureUnkConverter
  }

  private def tokenModifiers: List[data.TokenModifier] = InputOptions.tokenModifiers.map {
    case TokenModifier.numberCrusher => new NumberCrusher
    case TokenModifier.lowerCaser => new LowerCaser
  }.toList

  def vocab(dict: Dictionary) = ParserOptions.inputToken match {
    case InputToken.surface => dict.wordSize
    case InputToken.pos => dict.posSize
  }

  def tokenId2Str(dict: Dictionary): (Int=>String) = ParserOptions.inputToken match {
    case InputToken.surface => dict.word(_).toString
    case InputToken.pos => dict.pos(_).toString
  }

  class TrainParserSelector(val dict: Dictionary, sentences: Seq[Sentence])
      extends ParserSelector {
    val param = emptyParamFun(sentences, vocab(dict))()
  }

  class TestParserSelector(val dict: Dictionary, val param: DMVParameter)
      extends ParserSelector

  trait ParserSelector { outer=>
    val dict: Dictionary
    val param: DMVParameter
    def numVocab = vocab(dict)

    def mkExpectingParser(): ExpectingParser[DMVParameter] =
      (TrainingOptions.satisfiedLink, TrainingOptions.unleafPOS) match {
        case (SatisfiedLink.none, UnleafPOS.none) =>
          new BasicParser with LeftCornerSplitHeadExpectingParser[DMVParameter]
        case (_, _) =>
          new BasicParser with LeftCornerSplitHeadExpectingCondParser[DMVParameter]
      }

    def mkViterbiParser(): ViterbiParser =
      new BasicParser with LeftCornerSplitHeadViterbiParser[DMVParameter]

    def mkHarmonicParser(): ExpectingParser[DMVParameter] =
      new HarmonicParser with LeftCornerSplitHeadExpectingParser[DMVParameter]

    val _mkEmptyParam: ()=>DMVParameter = param.empty

    val mkBasicParamHolder: (Sentence, DMVParameter)=>ParamHolder[DMVParameter] =
      (sentence, partial) => new NoConstraintParamHolder(sentence, param, partial)
          with Constraints with DMVParamHolder

    val mkHarmonicParamHolder: (Sentence, DMVParameter) => ParamHolder[DMVParameter] =
      (sentence, partial) => new NoConstraintParamHolder(sentence, param, partial)
          with EmptyConstraints with HarmonicParamHolder

    trait CommonParser extends Parser[DMVParameter] {
      val param = outer.param
      def mkPartial(n: Int) = _mkPartial(n)
      def mkEmptyParam() = _mkEmptyParam()

      def mkDepthCalculator(sentence: Sentence) = _mkDepthCalculator(sentence)
      def calcMaxDepth(n: Int) = _calcMaxDepth(n)
      def calcMaxChunkSize(n: Int) = _calcMaxChunkSize(n)
      def mkCondition(sentence: Sentence) = _mkCondition(sentence)
      def mkLeafCondition(sentence: Sentence) = _mkLeafCondition(sentence)
    }

    trait BasicParser extends CommonParser {
      def mkParamHolder(sentence: Sentence, param: DMVParameter) =
        mkBasicParamHolder(sentence, param)
    }

    trait HarmonicParser extends CommonParser {
      def mkParamHolder(sentence: Sentence, param: DMVParameter) =
        mkHarmonicParamHolder(sentence, param)
    }
  }

  val _mkPartial: Int=>PartialCounts = new PartialCounts(_)

  val _calcMaxDepth: Int=>Int = _ => ParserOptions.maxDepth
  val _calcMaxChunkSize: Int=>Int =
    if (ParserOptions.maxDepth == 0) _ => UnlimitedDepthCalculator.maxChunkSize
    else _ => ParserOptions.maxChunkSize
  // val maxDepth =
  //   if (sentence.size < 10) 1
  //   else 2

  val _mkDepthCalculator: Sentence=>SplitHeadDepthCalculator =
    ParserOptions.chunkDef match {
      case ChunkDef.basic => sentence =>
        _calcMaxDepth(sentence.size) match {
          case 0 => UnlimitedDepthCalculator
          case d => new LimitedDepthCalculator(ParserOptions.maxChunkSize)
        }
      case ChunkDef.content => sentence =>
        _calcMaxDepth(sentence.size) match {
          case 0 => UnlimitedDepthCalculator
          case d => new LimitedDepthWithCheckCalculater(ParserOptions.maxChunkSize, sentence)
        }
    }

  val _mkCondition: Sentence=>LinkSatisfactionChecker =
    TrainingOptions.satisfiedLink match {
      case SatisfiedLink.none => _ => UnrestrictedLinkChecker
      case SatisfiedLink.verbNoun =>
        val cond: (PoS, PoS) => Boolean = (_.isVerb && _.isNoun)
        new POSBasedSatisfactionChecker(_, cond)
    }

  val _mkLeafCondition: Sentence=>LeafCondCalculator =
    TrainingOptions.unleafPOS match {
      case UnleafPOS.none => _ => NoLeafCondCalculator
      case UnleafPOS.adp => LeafCondCalculator.adpOnlyConstraintGen
      case UnleafPOS.adpPrtConj =>
        LeafCondCalculator.posBasedCalcGen(Set("ADP", "PRT"))
    }


  abstract class NoConstraintParamHolder(
    val sentence: Sentence,
    val param: DMVParameter,
    val partial: DMVParameter) extends ParamHolder[DMVParameter] {
    lazy val token = tokenFun(sentence)
  }

  trait EmptyConstraints {
    val distanceCost = ParamHolder.NoneDistanceCostCalculator
    val proceedCost = ParamHolder.NoProceedCostCalculator
    val compCost = ParamHolder.NoneCompCostCalculator
    val linkPOSConstraint = ParamHolder.NoLinkPOSConstraint
    val rootPOSConstraint = ParamHolder.NoRootConstrait
  }

  trait Constraints {
    val sentence: Sentence

    val distanceCost = (ParserOptions.maxDepLength, ParserOptions.distanceBeta) match {
      case (0, 0.0) => ParamHolder.NoneDistanceCostCalculator
      case (d, 0.0) => new ParamHolder.HardDistanceCostCalculator(d)
      case (0, b) =>
        new ParamHolder.ExpDistanceCostCalculator(b, ParserOptions.distanceOffset)
      case (d, b) => new ParamHolder.LimitedExpDistanceCostCalculator(d, b, ParserOptions.distanceOffset)
    }

    val proceedCost = {
      val unheadPOS: Set[String] = ParserOptions.unheadPOS match {
        case UnheadPOS.none => Set()
        case UnheadPOS.udfunc => Set("ADP", "AUX", "CONJ", "DET", "PART", "SCONJ")
        case UnheadPOS.detConj => Set("DET", "CONJ")
        case UnheadPOS.detPrt => Set("DET", "PRT")
        case UnheadPOS.detConjPrt => Set("DET", "CONJ", "PRT")
      }

      if (unheadPOS.isEmpty) ParamHolder.NoProceedCostCalculator
      else {
        val gen = new ParamHolder.UnheadProceedCostCalcGen(unheadPOS)
        gen.mkCalc(sentence)
      }
    }

    val compCost = ParserOptions.compBeta match {
      case 0.0 => ParamHolder.NoneCompCostCalculator
      case b => new ParamHolder.ExpCompCostCalculator(
        sentence.size, b, ParserOptions.compOffset)
    }

    val linkPOSConstraint = ParserOptions.linkPOSConstraint match {
      case LinkPOSConstraint.none => ParamHolder.NoLinkPOSConstraint
      case LinkPOSConstraint.functionLeave =>
        val gen = ParamHolder.LinkPOSConstraint.functionWordMustBeLeaveConstraint()
        gen(sentence)
    }

    lazy val rootPOSConstraint = ParserOptions.rootPOS match {
      case RootPOSConstraint.none => ParamHolder.NoRootConstrait
      case RootPOSConstraint.verb => new ParamHolder.RootVerbConstraint(sentence)
      case RootPOSConstraint.verbAndNoun => new ParamHolder.RootBothVerbAndNounConstraint(sentence)
      case RootPOSConstraint.verbOrNoun => new ParamHolder.RootVerbOrNounConstraint(sentence)
      case RootPOSConstraint.verbNounAdj => new ParamHolder.RootVerbNounAdjConstraint(sentence)
    }
  }

  def tokenFun(sentence: Sentence): Int=>Int = ParserOptions.inputToken match {
    case InputToken.surface =>
      sentence.word(_).id
    case InputToken.pos =>
      sentence.pos(_).id
  }

  /** This method may be customized if new parameter is added?
    */
  def emptyPartialFun(): Int=>PartialCounts = {
    size: Int => new PartialCounts(size)
  }

  def emptyParamFun(sentences: Seq[Sentence], numVocab: Int): ()=>DMVParameter = {

    def activeWordPairs = {

      val headToDepSet = Array.tabulate(numVocab) { _ => new HashSet[Int] }

      sentences foreach { s =>
        val token = tokenFun(s)
        (0 until s.size) foreach { i =>
          (i + 1 until s.size) foreach { j =>
            val (v, w) = (token(i), token(j))
            headToDepSet(v) += w
            headToDepSet(w) += v
          }
        }
      }
      headToDepSet.map { _.toArray.sortWith(_<_) }
    }

    val alpha = TrainingOptions.trainMethod match {
      case TrainMethod.em =>
        println("paramter smoothing: None")
        0.0
      case TrainMethod.viterbi =>
        println("parameter smoothing: laplace smoothing")
        1.0
    }
    ParserOptions.inputToken match {
      case InputToken.surface =>
        println("input token: surface => use sparse representation")
          () => new SparseDMVParameter(numVocab, alpha, alpha)
      case InputToken.pos =>
        println("input token: pos => use dense representation")
        TrainingOptions.filterWithActiveWordPairs match {
          case true =>
            println("filter with active word pairs: true")
              () => new FilteredNaiveDMVParameter(activeWordPairs, alpha, alpha)
          case false =>
            println("filter with active word pairs: false")
              () => new NaiveDMVParameter(numVocab, alpha, alpha)
        }
    }
  }
}
