package lcdmv.data

trait PoSManager extends StringBaseNumberedManager[PoS] with UnkObjectReturner[PoS] {
  override def unknown = sys.error("Not supported unknown token") // getOrCreate("UNK_POS")
  override type GetType = PoS

  var posToUpos = Map.empty[String, String]

  def toUpos(pos: String) = posToUpos.getOrElse(pos, "_")
}

class SimplePoSManager extends PoSManager {
  override def createWithId(original: PoS) = SimplePoS(newId, original.v, original.upos)
  override def createCanonicalInstance(str: String) = {
    SimplePoS(0, str, toUpos(str))
    // val splitted = str.split("\t")
    // SimplePoS(0, splitted(0), splitted(1))
  }
}
