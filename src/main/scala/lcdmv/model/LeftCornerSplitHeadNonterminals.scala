package lcdmv.model

object LeftCornerSplitHeadNonterminals {

  val numNonterminals = 8

  case class CondNonterm(nonterm: Nonterminal, cond: Boolean)

  sealed trait Nonterminal {
    def id: Int
    def i: Int
    def j: Int
    def k = 0
    def d = 0
    def m = 0

    def predHasChild = true

    def p = if (predHasChild) 0 else 1
    // def noPredChild =  // 0 = have child (or does not care), 1 = do not have
  }

  case class S(begin: Int, head: Int, end: Int)
      extends Nonterminal {
    def id = 0
    def i = begin
    def j = head
    override def k = end
  }

  case class Triangle(begin: Int, head: Int, end: Int, override val d: Int) extends Nonterminal {
    def id = 1
    def i = begin
    def j = head
    override def k = end
  }

  case class LeftFull(edge: Int, head: Int, override val d: Int) extends Nonterminal {
    def id = 2
    def i = edge
    def j = head
  }

  case class RightFull(head: Int, edge: Int, override val d: Int)
      extends Nonterminal {
    def id = 3
    def i = head
    def j = edge
  }

  case class SquareFullPred(begin: Int, end: Int, pred: Int, override val d: Int) extends Nonterminal {
    def id = 4
    def i = begin
    def j = end
    override def k = pred
  }

  case class SquareFragPred(
    begin: Int,
    end: Int,
    pred: Int,
    override val d: Int,
    override val m: Int) extends Nonterminal {
    assert(m == 0 || m < end - begin)
    def id = 5
    def i = begin
    def j = end
    override def k = pred
  }

  case class RightFullPred(
    head: Int,
    edge: Int,
    pred: Int,
    override val d: Int,
    override val predHasChild: Boolean = true
  ) extends Nonterminal {
    def id = 6
    def i = head
    def j = edge
    override def k = pred
  }

  case class RightFragPred(
    head: Int,
    edge: Int,
    pred: Int,
    override val d: Int,
    override val m: Int) extends Nonterminal {
    assert(m == 0 || m < edge - head)
    val id = 7
    def i = head
    def j = edge
    override def k = pred
  }
}
