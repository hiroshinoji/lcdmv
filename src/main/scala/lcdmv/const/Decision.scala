package lcdmv.const

object Decision {
  case object stop extends Decision(0)
  case object proceed extends Decision(1)
}

sealed abstract class Decision(val id: Int)
