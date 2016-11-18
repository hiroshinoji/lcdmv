package lcdmv

import annotation.meta.getter
import OptionEnumTypes._

trait Options {
  type Option = fig.basic.Option @getter
  type OptionSet = fig.basic.OptionSet @getter
}

object DriverOptions extends Options {
  //import OptionEnumTypes.ActionType

  @Option(gloss="Running action", required=true) var actionType:ActionType = _
}

object InputOptions extends Options {
  //import OptionEnumTypes.{Format, HowToReadAtTrain, UnkConverter}

  @Option(gloss = "Path to training corpus path") var trainPath = ""
  @Option(gloss = "File format") var fileFormat = Format.CoNLLX
  @Option(gloss = "Training instances, 0 for all") var trainSize = 0
  @Option(gloss = "Path to test corpus for evlauation") var testPath = ""
  // @Option(gloss = "Path to develop Treebank for evlauation") var developPath = ""
  @Option(gloss = "Test instances, 0 for all") var testSize = 0
  @Option(gloss = "Maximum sentence length") var maxLength = 10
  @Option(gloss = "Mimimum sentence length") var minLength = 0

  @Option(gloss = "How to read corpus at training? (active means calculating vocaborary only from filtered sentences with e.g., maxLength)") var howToRead = HowToReadAtTrain.allTrain

  @Option(gloss = "Include test data as training data") var includeTest = true
  @Option(gloss = "Remove punctuations (. in universal tag))") var removePunc = true

  @Option(gloss = "Path to trained model") var loadModelPath = ""

  @Option(gloss = "Token modifiers") var tokenModifiers = Array(TokenModifier.numberCrusher, TokenModifier.lowerCaser)

  @Option(gloss = "UNK threshold (words which # occurence is below this are replaced by UNK)") var unkThreshold = 0
  @Option(gloss = "Unknown converter") var unkConverter = UnkConverter.simple
}

object OutputOptions extends Options {
  @Option(gloss = "Path to output of trained model after the training") var saveModelPath = ""
  @Option(gloss = "Path to write trained parser model in a readable form") var parserFeaturePath = ""
  @Option(gloss = "Path to output parser predictions against test data") var outputPath = ""
  @Option(gloss = "Output format (for CoNLLX coarse tags are used as universal tags)") var outputFormat = Format.CoNLLX
  @Option(gloss = "Path to output analysis result") var analysisPath = ""
}

object TrainingOptions extends Options {
  //import OptionEnumTypes.{ TrainMethod, Initializer }

  @Option(gloss="Number of iterations, 0 for until likelihood unchanged") var numIters:Int = 10
  @Option(gloss="Number of threads for parallel EM") var numThreads = 20

  @Option(gloss="Shuffle with a random seed") var shuffle = true
  @Option(gloss="Training method") var trainMethod = TrainMethod.em
  @Option(gloss="Initializer") var initializer = Initializer.uniform
  @Option(gloss="Reset root to uniform distribution") var uniformRoot = false
  @Option(gloss="Normalize after the initial E-step") var doNormalize = true

  @Option(gloss="Filter with active word pairs (should be true to reproduce the score in Berg-kirkpatric et al. (2010); Now only supports with pos inputs)") var filterWithActiveWordPairs = false

  @Option(gloss="Link combination that must be appead at least once in the chart") var satisfiedLink = SatisfiedLink.none
  @Option(gloss="POS tags that cannot be leaf node") var unleafPOS = UnleafPOS.none
}

object DictionaryOptions extends Options {
}

object ParserOptions extends Options {
  //import OptionEnumTypes.InputToken

  @Option(gloss="The definition of a token") var inputToken = InputToken.pos
  @Option(gloss="The definition of POS") var posdef = Posdef.pos
  @Option(gloss="Max stack depth, 0 for unlimited (original DMV)") var maxDepth = 0
  @Option(gloss="Max dependency length") var maxDepLength = 0
  @Option(gloss="Max span length recognized as center-embedding (minimum=1; if 2, two words chunk is allowed at the bottom of embedding)") var maxChunkSize = 1
  //@Option(gloss="Min span length of center-embedding (2 means only a length 1 span (a word) is recognized as not-center-embedded construction)") var minSpanLength = 2
  @Option(gloss="Root POS constraint") var rootPOS = RootPOSConstraint.none
  @Option(gloss="Type of link candidate restriction based on UPOS") var linkPOSConstraint = LinkPOSConstraint.none

  @Option(gloss="Hyper of distance cost") var distanceBeta = 0.0
  @Option(gloss="Offset of soft (exp) distance cost") var distanceOffset = 1
  @Option(gloss="Hyper of comp cost") var compBeta = 0.0
  @Option(gloss="Offset of soft (exp) comp cost") var compOffset = 0

  @Option(gloss="Which pos is prohibited to have a dependent (can only be a leaf)") var unheadPOS = UnheadPOS.none

  @Option(gloss="How to calculate chunk length") var chunkDef = ChunkDef.basic
}

object ReguralizeOptions extends Options {
  @Option(gloss="Size of batches to use if useStochastic and you give a BatchDiffFunction") var batchSize = 512
  @Option(gloss="Regularization constant to use") var regularization = 10
  @Option(gloss="Rate of change to use, only applies to SGD") var alpha = 0.5
  // @Option(gloss="How many iterations to do") var numIterations = 10
  @Option(gloss="If true, use L1 regularization. Otherwise, use L2") var useL1 = false
  @Option(gloss="Convergence tolerance, looking at both average improvement and the norm of the gradient") var tolerance = 1e-5
  @Option(gloss="If false, use LBFGS or OWLQN. If true, use some variant of Stochastic Gradient Descent") var useStochastic = false
}
