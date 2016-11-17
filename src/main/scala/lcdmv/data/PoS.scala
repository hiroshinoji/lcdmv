package lcdmv.data

sealed trait PoS extends Numbered[String] {
  def upos = ""
  def isVerb = upos == "VERB"
  def isNoun = upos == "NOUN" || upos == "PRON" || upos == "PROPN" // || upos == "NUM"
  def isAdj = upos == "ADJ"

  def isFunc = PoS.funcPOS.contains(upos)
}

object PoS {
  val funcPOS = Set("ADP", "AUX", "CONJ", "DET", "PART", "SCONJ", "PRT")
}

@SerialVersionUID(1L)
case class SimplePoS(override val id:Int, override val v:String, override val upos: String = "") extends PoS {
  override def toString = v
}
