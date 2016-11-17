package lcdmv.model

import org.scalatest._

class MidlyLimitedSplitHeadParserSpec
    extends FlatSpec with Matchers with SplitHeadParserSetting {

  def mkChart(n: Int, d: Int, m: Int, activeLinks: (Int, Int)*) = {
    val gen = new BiasedParserGen(n)
    gen.setBiasedParam(activeLinks:_*)
    gen.parser(d, LimitedDepthCalculator(m)).mkChart(gen.sentence)
  }
  def mkUniformChart(n: Int, d: Int, m: Int) = {
    val gen = new BiasedParserGen(n)
    gen.setUniformParam()
    gen.parser(d, LimitedDepthCalculator(m)).mkChart(gen.sentence)
  }

  "Chart's incrementCounts" should "find an argmax tree and increment only parameteres used there (a->b)" in {

    val chart = mkChart(2, 1, 2, (0, 1))
    chart.incrementCounts()

    val param = chart.partial

    param.root(0) should equal (one)
    param.root(1) should equal (0)
    param.choice(0, Right)(1) should equal (one)
    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).proceed should equal (one)
    param.gen(0, Right, HasChild).stop should equal (one)
    param.gen(1, Left, NoChild).stop should equal (one)
    param.gen(1, Right, NoChild).stop should equal (one)
  }

  it should "do the same thing with another parameterization (a->b;a->c)" in {
    val chart = mkChart(3, 1, 2, (0, 1), (0, 2))
    chart.incrementCounts()

    val param = chart.partial
    val manualParam = param.empty

    manualParam.incrementRoot(0, 1.0)
    manualParam.incrementChoice(0, Right, 1, 1.0)
    manualParam.incrementChoice(0, Right, 2, 1.0)
    manualParam.incrementStop(0, Left, NoChild, 1.0)
    manualParam.incrementProceed(0, Right, NoChild, 1.0)
    manualParam.incrementProceed(0, Right, HasChild, 1.0)
    manualParam.incrementStop(0, Right, HasChild, 1.0)
    manualParam.incrementStop(1, Left, NoChild, 1.0)
    manualParam.incrementStop(1, Right, NoChild, 1.0)
    manualParam.incrementStop(2, Left, NoChild, 1.0)
    manualParam.incrementStop(2, Right, NoChild, 1.0)

    equalParams(param, manualParam)
  }

  "inside of root" should "be the same as outside of terminal if no tree is discarded" in {
    // when m = 2, all tree with depth <= 1 should be recognized
    val chart = mkUniformChart(4, 1, 2)
    chart.terminalOutside(0) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(1) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(2) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(3) should be (2 * chart.marginal +- 1e-10)
  }

  "Chart with stack bound <= 1 (2)" should "recognize constructions which require depth=1 (1)" in {
    val chart1 = mkChart(4, 1, 2, (0, 3), (3, 1), (1, 2))
    chart1.marginal should (be > 0.0)

    val chart2 = mkChart(4, 1, 2, (0, 1), (1, 2), (1, 3))
    chart2.marginal should (be > 0.0)

    val chart3 = mkChart(4, 1, 2, (3, 0), (3, 1), (1, 2))
    chart3.marginal should (be > 0.0)
  }

  "Chart with stack bound <= 1 (2)" should "not recognize constructions which require depth=1 (2)" in {
    val chart1 = mkChart(5, 1, 2, (0, 4), (4, 1), (1, 2), (2, 3))
    chart1.marginal should be (0.0)

    val chart2 = mkChart(5, 1, 2, (0, 4), (4, 1), (1, 3), (3, 2))
    chart2.marginal should be (0.0)

    val chart3 = mkChart(5, 1, 2, (0, 1), (1, 2), (2, 3), (1, 4))
    chart3.marginal should be (0.0)

    val chart4 = mkChart(5, 1, 2, (0, 1), (1, 4), (1, 3), (3, 2))
    chart4.marginal should be (0.0)

    val chart5 = mkChart(5, 1, 2, (4, 0), (4, 1), (1, 2), (2, 3))
    chart5.marginal should be (0.0)

    val chart6 = mkChart(5, 1, 2, (4, 0), (4, 1), (1, 3), (3, 2))
    chart6.marginal should be (0.0)
  }

  "Chart with stack bound <= 1 (3)" should "be able to recognize these trees" in {
    val chart1 = mkChart(5, 1, 3, (0, 4), (4, 1), (1, 2), (2, 3))
    chart1.marginal should be (1.0)

    val chart2 = mkChart(5, 1, 3, (0, 4), (4, 1), (1, 3), (3, 2))
    chart2.marginal should be (1.0)

    val chart3 = mkChart(5, 1, 3, (0, 1), (1, 2), (2, 3), (1, 4))
    chart3.marginal should be (1.0)

    val chart4 = mkChart(5, 1, 3, (0, 1), (1, 4), (1, 3), (3, 2))
    chart4.marginal should be (1.0)

    val chart5 = mkChart(5, 1, 3, (4, 0), (4, 1), (1, 2), (2, 3))
    chart5.marginal should be (1.0)

    val chart6 = mkChart(5, 1, 3, (4, 0), (4, 1), (1, 3), (3, 2))
    chart6.marginal should be (1.0)

  }

  it should "not recognize constructions which require depth=2" in {
    val chart1 = mkChart(6, 1, 3, (0, 5), (5, 1), (1, 4), (4, 2), (2, 3))
    chart1.marginal should be (0.0)
  }

  it should "not recognize constructions which require depth=1 (4)" in {
    val chart1 = mkChart(6, 1, 3, (0, 5), (5, 1), (1, 2), (2, 3), (3, 4))
    chart1.marginal should be (0.0)
  }

  "Chart with stack bound <= 1 (4)" should "be able to recognize the tree" in {
    val chart1 = mkChart(6, 1, 4, (0, 5), (5, 1), (1, 2), (2, 3), (3, 4))
    chart1.marginal should be (1.0)
  }
}
