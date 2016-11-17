package lcdmv.data

/** This dictionary preprocess all input tokens with some token modifiers.
  * WordManager assumes the input is already preprocessed. This is because
  * doing preprocess in WordManager class makes internal mechanism quite complicated,
  * which is my first attempt but failed.
  */
@SerialVersionUID(1L)
trait Dictionary extends Serializable {

  import Dictionary._

  protected def wordManager: WordManager
  protected def posManager: PoSManager

  def tokenModifiers: List[TokenModifier]

  def preprocess(token: String) = applyModifiers(token, tokenModifiers)

  private def applyModifiers(current: String, remains: List[TokenModifier]): String = remains match {
    case Nil => current
    case head :: tail => applyModifiers(head(current), tail)
  }

  private[this] def combinePoS(pos: String, upos: String) = pos + "%%###%%" + upos

  def posOrCreate(str: String): PoS = posManager.getOrCreate(str)
  // def posOrCreate(str: String, uposStr: String) = posManager.getOrCreate(combinePoS(str, uposStr))

  def pos(str: String): PoS = posManager.get(str)
  // def pos(str: String, uposStr: String) = posManager.getOrCreate(combinePoS(str, uposStr))

  // It might not be very efficient to check whether id is root id every time but we also assume this method is not called in the core part of parser.
  def pos(id: Int): PoS = if (id == rootPoSID) rootPoS else posManager(id)

  // def rootPoS: PoS = posManager.getOrCreate("_ROOT_POS_")
  def emptyPoS: PoS = posManager.getOrCreate("")

  def wordOrCreate(str: String): Word = wordManager.getOrCreate(preprocess(str))
  def word(str: String): Word = wordManager.get(preprocess(str))
  def word(id: Int): Word = if (id == rootWordID) rootWord else wordManager(id)

  /** This dictionary assumes an unknown word is type-dependent, that is,
    * an unknown token may be generated by extracting features from a token.
    * This method try to register all such possible unknown tokens from the
    * (unprocessed) words in the corpus.
    */
  def registUnknownWordsWith(tokens: Seq[String]) =
    wordManager.registUnknownWordsWith(tokens.map(preprocess(_)))

  def registPosMagging(map: Map[String, String]) = posManager.posToUpos = map

  // def rootWord: Word = wordManager.getOrCreate("_ROOT_WORD_")
  def emptyWord: Word = wordManager.getOrCreate("")

  def wordSize = wordManager.size
  def posSize = posManager.size

  override def toString = wordManager.toString // for debugging purpose
}

object Dictionary {
  val rootPoSID = -1
  val rootPoS: PoS = SimplePoS(rootPoSID, "_ROOT_POS_")

  val rootWordID = -1
  val rootWord = SimpleWord(rootWordID, "_ROOT_WORD_")
}

@SerialVersionUID(-5797904967429249190L)
class SimpleDictionary(
  override val wordManager: WordManager,
  override val posManager: PoSManager,
  override val tokenModifiers: List[TokenModifier]) extends Dictionary
