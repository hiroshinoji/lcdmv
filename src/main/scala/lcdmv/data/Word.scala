package lcdmv.data

trait Word extends Numbered[String] {
  // additional information is defined in function; may or may not be overridden in val by subclasses
  def classId:String = throw new RuntimeException("classId is not defined in this Word class.")
}

case class SimpleWord(override val id:Int, override val v:String) extends Word {
  override def toString = v
}
case class ClassedWord(override val id:Int,
                       override val v:String,
                       override val classId:String) extends Word {
  override def toString = v + "[" + classId + "]"
}
