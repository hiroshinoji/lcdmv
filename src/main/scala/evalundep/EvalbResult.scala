package evalundep

import scala.sys.process.Process

case class EvalbStat(recall: Double, prec: Double, matched: Int, gold: Int, test: Int, crossed: Int)

case class EvalbResult(stats: Seq[EvalbStat], totalStat: EvalbStat, f1: Double, complete: Double)

object EvalbResult {

  def fromCommand(cmd: String) = {
    val output = Process(cmd).lines_!.toIndexedSeq

    val begin = output.indexWhere(_.startsWith("======"))
    val end = output.indexWhere(_.startsWith("======"), begin + 1)

    val stats = output.view(begin + 1, end).map(_.trim.split("\\s+")).map {
      case Array(_, _, _, r, p, m, g, t, c) => EvalbStat(r.toDouble, p.toDouble, m.toInt, g.toInt, t.toInt, c.toInt)
    }
    val finalStat = output(end + 1).trim.split("\\s+") match {
      case Array(r, p, m, g, t, c, _*) =>
        EvalbStat(r.toDouble, p.toDouble, m.toInt, g.toInt, t.toInt, c.toInt)
    }

    val f1 = output(output.indexWhere(_.startsWith("Bracketing FMeasure"), end + 1)).split("\\s+").last.toDouble
    val complete = output(output.indexWhere(_.startsWith("Complete match"), end + 1)).split("\\s+").last.toDouble

    EvalbResult(stats, finalStat, f1, complete)
  }
}
