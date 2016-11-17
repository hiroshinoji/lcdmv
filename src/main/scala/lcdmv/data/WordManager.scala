package lcdmv.data

trait WordManager extends StringBaseNumberedManager[Word] with UnkWithTemplateReturner[Word] {

  override type GetType = Word

  def unkConverter: TokenModifier

  override def get(str: String) = super.get(str)
  override def getOrCreate(str: String) = super.getOrCreate(str)

  override def extractTemplate(original: String): String = unkConverter(original)

  /** This method try to add all possible templates for a future unknown word.
    * WARNING: If there are some patterns which is not covered by the given `tokens`,
    * it throws an exception with `unknown` method. So please take care that
    * argument `tokens` covers all the possible structures for a future word.
    */
  def registUnknownWordsWith(tokens: Seq[String]) = {
    tokens foreach { t => registTemplateFor(t) }
  }

  override val unknown = SimpleWord(-1, "#UNK#")
    //sys.error("Should never reach here. All unknown word should be handled by template extractor.")
}

@SerialVersionUID(1L)
class SimpleWordManager(override val unkConverter: TokenModifier) extends WordManager {

  override def createWithId(original: Word) = SimpleWord(newId, original.v)
  override def createCanonicalInstance(str: String) = SimpleWord(0, str)
}

// class SimpleWordManager extends WordManager {
//   override def createWithId(original: Word) = SimpleWord(newId, original.v)
//   override def createCanonicalInstance(str: String) = SimpleWord(0, str)
// }

// trait ClassedWordManager extends WordManager {
//   def word2class: (String => String) // word str -> class str (e.g. 121)
//   override def createWithId(original: Word) = ClassedWord(newId, original.v, original.classId)
//   override def createCanonicalInstance(str: String) = ClassedWord(0, str, word2class(str))
// }

// object WordManager {
//   def simpleManager = new SimpleWordManager

//   def classedManager(word2classMap: Map[String, String]) = new ClassedWordManager {
//     override def word2class = { wordStr => word2class(wordStr) }
//   }
// }
