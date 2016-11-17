package lcdmv.const

object Direction {
  case object left extends Direction(0)
  case object right extends Direction(1)

  def apply(id: Int) = if (id == 0) left else right
}

sealed abstract class Direction(val id: Int)
