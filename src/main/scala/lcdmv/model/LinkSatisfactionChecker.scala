package lcdmv.model

import lcdmv.data.{PoS, Sentence}

trait LinkSatisfactionChecker {
  def satisfy(head: Int, dep: Int): Boolean
}

object UnrestrictedLinkChecker extends LinkSatisfactionChecker {
  def satisfy(head: Int, dep: Int) = true
}

class POSBasedSatisfactionChecker(
  val sentence: Sentence,
  val cond: (PoS, PoS) => Boolean) extends LinkSatisfactionChecker {

  val existLink: Boolean = {
    val links = for (
      i <- (0 until sentence.size);
      j <- (i + 1 until sentence.size)
    ) yield (i, j)
    links.exists { l => check(l._1, l._2) || check(l._2, l._1) }
  }

  private[this] val satisfyFun: (Int, Int) => Boolean =
    if (!existLink) (_, _) => true
    else {
      val map: Array[Array[Boolean]] = (0 until sentence.size).map { i =>
        (0 until sentence.size).map { j =>
          if (i == j) false
          else check(i, j)
        }.toArray
      }.toArray
      map(_)(_)
    }

  def check(head: Int, dep: Int) =
    cond(sentence.pos(head), sentence.pos(dep))

  def satisfy(head: Int, dep: Int) = satisfyFun(head, dep)
}
