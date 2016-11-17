package lcdmv.model

import org.scalatest._

class LeafCondParserSpec extends FlatSpec with Matchers with SplitHeadParserSetting {

  def mkChart(n: Int, unleafPOS: Set[String], activeLinks: (Int, Int)*) = {
    val gen = new BiasedParserGen(n)
    gen.setBiasedParam(activeLinks:_*)

    val leafCond =
      if (unleafPOS.isEmpty) NoLeafCondCalculator
      else LeafCondCalculator.posBasedCalcGen(unleafPOS)(gen.sentence)

    gen.leafCondParser(leafCond).mkChart(gen.sentence)
  }

  def mkUniformChart(n: Int, unleafPOS: Set[String]) = {
    val gen = new BiasedParserGen(n)
    gen.setUniformParam()

    val leafCond =
      if (unleafPOS.isEmpty) NoLeafCondCalculator
      else LeafCondCalculator.posBasedCalcGen(unleafPOS)(gen.sentence)

    gen.leafCondParser(leafCond).mkChart(gen.sentence)
  }

  "inside of root" should "be the same as outside of terminal when all tokens are leafable" in {
    val chart = mkUniformChart(5, Set("g"))
    chart.terminalOutside(0) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(1) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(2) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(3) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(4) should be (2 * chart.marginal +- 1e-10)
  }

  "Restricted chart" should "find an argmax tree if condition is satisfied" in {

    val chart = mkChart(4, Set("a"), (0, 1), (1, 2), (1, 3))
    chart.incrementCounts()

    import LeftCornerSplitHeadNonterminals._

    val param = chart.partial

    chart.marginal should be (1.0)

    param.root(0) should equal (one)
    param.choice(0, Right)(1) should equal (one)
    param.choice(1, Right)(2) should equal (one)
    param.choice(1, Right)(3) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).proceed should equal (one)
    param.gen(0, Right, HasChild).stop should equal (one)

    param.gen(1, Left, NoChild).stop should equal (one)
    param.gen(1, Right, NoChild).proceed should equal (one)
    param.gen(1, Right, HasChild).proceed should equal (one)
    param.gen(1, Right, HasChild).stop should equal (one)

    param.gen(2, Left, NoChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  it should "fail to find tree if condition cannot be satisifed" in {

    val chart = mkChart(4, Set("c"), (0, 1), (1, 2), (1, 3))
    chart.marginal should be (0.0)

    chart.incrementCounts()

    // import LeftCornerSplitHeadNonterminals._
    // chart.leafCond.logicallyOK(RightFullPred(1, 1, 2, 0, true)) should be (false)

    val param = chart.partial

    val emptyParam = param.empty
    equalParams(param, emptyParam)
  }

  it should "behaves usually for one word sentence" in {

    val chart = mkChart(1, Set("a"))
    chart.marginal should be (1.0)

    chart.incrementCounts()

    val param = chart.partial

    val manualParam = param.empty
    manualParam.incrementRoot(0, 1.0)
    manualParam.incrementStop(0, Left, NoChild, 1.0)
    manualParam.incrementStop(0, Right, NoChild, 1.0)

    equalParams(param, manualParam)
  }

  it should "decrease scores corresponding to unsatisfied trees (a cannot be leaf)" in {

    val chart = mkChart(3, Set("a"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (4.0)

    param.root(0) should equal (3.0/4.0)
    param.root(1) should equal (0.0)
    param.root(2) should equal (1.0/4.0)

    param.choice(0, Right)(1) should equal (3.0/4.0)
    param.choice(0, Right)(2) should equal (2.0/4.0)
    param.choice(1, Left)(0) should equal (0.0)
    param.choice(1, Right)(2) should equal (1.0/4.0)
    param.choice(2, Left)(0) should equal (1.0/4.0)
    param.choice(2, Left)(1) should equal (1.0/4.0)

    param.gen(0, Left, NoChild).stop should equal (1.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (0.0)
    param.gen(0, Right, NoChild).proceed should equal (1.0)
    param.gen(0, Right, HasChild).stop should equal (1.0)
    param.gen(0, Right, HasChild).proceed should equal (1.0/4.0)

    param.gen(1, Left, NoChild).stop should equal (1.0)
    param.gen(1, Left, NoChild).proceed should equal (0.0)
    param.gen(1, Left, HasChild).stop should equal (0.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (3.0/4.0)
    param.gen(1, Right, NoChild).proceed should equal (1.0/4.0)
    param.gen(1, Right, HasChild).stop should equal (1.0/4.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (2.0/4.0)
    param.gen(2, Left, NoChild).proceed should equal (2.0/4.0)
    param.gen(2, Left, HasChild).stop should equal (2.0/4.0)
    param.gen(2, Left, HasChild).proceed should equal (0.0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

  it should "decrease scores corresponding to unsatisfied trees (b cannot be leaf)" in {

    val chart = mkChart(3, Set("b"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (3.0)

    param.root(0) should equal (1.0/3.0)
    param.root(1) should equal (1.0/3.0)
    param.root(2) should equal (1.0/3.0)

    param.choice(0, Right)(1) should equal (1.0/3.0)
    param.choice(0, Right)(2) should equal (0.0)
    param.choice(1, Left)(0) should equal (2.0/3.0)
    param.choice(1, Right)(2) should equal (2.0/3.0)
    param.choice(2, Left)(0) should equal (0.0)
    param.choice(2, Left)(1) should equal (1.0/3.0)

    param.gen(0, Left, NoChild).stop should equal (3.0/3.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (2.0/3.0)
    param.gen(0, Right, NoChild).proceed should equal (1.0/3.0)
    param.gen(0, Right, HasChild).stop should equal (1.0/3.0)
    param.gen(0, Right, HasChild).proceed should equal (0.0)

    param.gen(1, Left, NoChild).stop should equal (1.0/3.0)
    param.gen(1, Left, NoChild).proceed should equal (2.0/3.0)
    param.gen(1, Left, HasChild).stop should equal (2.0/3.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (1.0/3.0)
    param.gen(1, Right, NoChild).proceed should equal (2.0/3.0)
    param.gen(1, Right, HasChild).stop should equal (2.0/3.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (2.0/3.0)
    param.gen(2, Left, NoChild).proceed should equal (1.0/3.0)
    param.gen(2, Left, HasChild).stop should equal (1.0/3.0)
    param.gen(2, Left, HasChild).proceed should equal (0.0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

  it should "decrease scores corresponding to unsatisfied trees (c cannot be leaf)" in {

    val chart = mkChart(3, Set("c"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (4.0)

    param.root(0) should equal (1.0/4.0)
    param.root(1) should equal (0.0)
    param.root(2) should equal (3.0/4.0)

    param.choice(0, Right)(1) should equal (1.0/4.0)
    param.choice(0, Right)(2) should equal (1.0/4.0)
    param.choice(1, Left)(0) should equal (1.0/4.0)
    param.choice(1, Right)(2) should equal (0.0)
    param.choice(2, Left)(0) should equal (2.0/4.0)
    param.choice(2, Left)(1) should equal (3.0/4.0)

    param.gen(0, Left, NoChild).stop should equal (1.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (2.0/4.0)
    param.gen(0, Right, NoChild).proceed should equal (2.0/4.0)
    param.gen(0, Right, HasChild).stop should equal (2.0/4.0)
    param.gen(0, Right, HasChild).proceed should equal (0.0)

    param.gen(1, Left, NoChild).stop should equal (3.0/4.0)
    param.gen(1, Left, NoChild).proceed should equal (1.0/4.0)
    param.gen(1, Left, HasChild).stop should equal (1.0/4.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (1.0)
    param.gen(1, Right, NoChild).proceed should equal (0.0)
    param.gen(1, Right, HasChild).stop should equal (0.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (0.0)
    param.gen(2, Left, NoChild).proceed should equal (1.0)
    param.gen(2, Left, HasChild).stop should equal (1.0)
    param.gen(2, Left, HasChild).proceed should equal (1.0/4.0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

  it should "correctly decrease scores when # unleafable nodes is 2" in {

    val chart = mkChart(4, Set("b", "c"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (4.0)

    param.root(0) should equal (1.0/4.0)
    param.root(1) should equal (1.0/4.0)
    param.root(2) should equal (1.0/4.0)
    param.root(3) should equal (1.0/4.0)

    param.choice(0, Right)(1) should equal (1.0/4.0)
    param.choice(0, Right)(2) should equal (0.0)
    param.choice(0, Right)(3) should equal (0.0)
    param.choice(1, Left)(0) should equal (3.0/4.0)
    param.choice(1, Right)(2) should equal (2.0/4.0)
    param.choice(1, Right)(3) should equal (0.0)
    param.choice(3, Left)(0) should equal (0.0)
    param.choice(3, Left)(1) should equal (0.0)
    param.choice(3, Left)(2) should equal (1.0/4.0)
  }

}
