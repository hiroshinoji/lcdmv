package lcdmv.model

import org.scalatest._

class ViterbiParserSpec extends FlatSpec with Matchers with SplitHeadParserSetting {

  // def mkChart(n: Int, d: Int, m: Int) = {
  //   val gen = new BiasedParserGen(n)
  //   gen.setUniformParam()
  //   val c = d match {
  //     case 0 => gen.viterbiParser(0, UnlimitedDepthCalculator)
  //     case _ => gen.viterbiParser(d, new LimitedDepthCalculator(m))
  //   }
  // }

  "No constraint chart's parse" should "return the viterbi parse" in {

    val gen = new BiasedParserGen(5)
    gen.setUniformParam()

    gen.addLinkWeight(0, 4, 1.0)
    gen.addLinkWeight(4, 1, 1.0)
    gen.addLinkWeight(1, 2, 0.5)
    gen.addLinkWeight(2, 3, 1.0)

    val parser = gen.viterbiParser(0, UnlimitedDepthCalculator)
    val deparcs = parser.parse(gen.sentence).get._1

    deparcs.heads should be (Seq(-1, 4, 1, 2, 0))
  }

  "If constraint is given, the parser" should "be changed to the second best one" in {

    val gen = new BiasedParserGen(5)
    gen.setUniformParam()

    gen.addLinkWeight(0, 4, 1.0)
    gen.addLinkWeight(4, 1, 1.0)
    gen.addLinkWeight(1, 2, 0.5)
    gen.addLinkWeight(2, 3, 1.0)

    val parser = gen.viterbiParser(1, new LimitedDepthCalculator(2))
    val deparcs = parser.parse(gen.sentence).get._1

    deparcs.heads should be (Seq(-1, 4, 4, 2, 0))
  }
}
