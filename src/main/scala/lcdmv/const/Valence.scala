package lcdmv.const

object Valence {
  case object noChild extends Valence(0)
  case object hasChild extends Valence(1)

  def apply(noDep: Boolean) = if (noDep) noChild else hasChild
  def apply(noDep: Int) = if (noDep == 0) noChild else hasChild
}

sealed abstract class Valence(val id: Int)
