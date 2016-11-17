package lcdmv.data

@SerialVersionUID(1L)
trait TokenModifier extends Serializable {
  def apply(token: String): String
}

class SimpleUnkConverter extends TokenModifier {
  override def apply(token: String) = "__UNK__"
}

class SurfaceFeatureUnkConverter extends TokenModifier {
  override def apply(token: String) = {
    token
  }
}

/** Do nothing
  */
class OriginalKeeper extends TokenModifier {
  def apply(token: String) = token
}

class NumberCrusher extends TokenModifier {
  val num = """\d+,*\d*""".r
  override def apply(token: String) = num replaceAllIn (token, "NUMBER")
}

class LowerCaser extends TokenModifier {
  override def apply(token: String) = token.toLowerCase
}
