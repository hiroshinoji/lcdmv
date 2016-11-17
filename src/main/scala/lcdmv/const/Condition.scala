package lcdmv.const

object Condition {

  def id(d: Direction, v: Valence) = (d, v) match {
    case (Direction.left, Valence.noChild) => 0
    case (Direction.left, Valence.hasChild) => 1
    case (Direction.right, Valence.noChild) => 2
    case (Direction.right, Valence.hasChild) => 3
  }
  def dir(cond: Int) = cond match {
    case 0 | 1 => Direction.left
    case 2 | 3 => Direction.right
  }
  def valence(cond: Int) = cond match {
    case 0 | 2 => Valence.noChild
    case 1 | 3 => Valence.hasChild
  }

  case class GenCondition(head: Int, dir: Direction, v: Valence, dec: Decision)

  object GenCondition {
    def apply(h: Int, c: Int, dec: Decision): GenCondition = c match {
      case 0 => GenCondition(h, Direction.left, Valence.noChild, dec)
      case 1 => GenCondition(h, Direction.left, Valence.hasChild, dec)
      case 2 => GenCondition(h, Direction.right, Valence.noChild, dec)
      case 3 => GenCondition(h, Direction.right, Valence.hasChild, dec)
    }
  }

  case class ChoiceCondition(head: Int, dir: Direction, arg: Int)
}
