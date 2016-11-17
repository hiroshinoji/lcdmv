package lcdmv.model

import org.scalatest._

class CondParserSpec extends FlatSpec with Matchers with SplitHeadParserSetting {

  def mkChart(n: Int, link: Option[(String, String)], activeLinks: (Int, Int)*) = {
    val gen = new BiasedParserGen(n)
    gen.setBiasedParam(activeLinks:_*)

    val checker = link map { case (h, d) =>
      new POSBasedSatisfactionChecker(
        gen.sentence,
        _.v == h && _.v == d
      )
    } getOrElse (UnrestrictedLinkChecker)

    gen.condParser(checker).mkChart(gen.sentence)
  }
  def mkUniformChart(n: Int, checker: LinkSatisfactionChecker) = {
    val gen = new BiasedParserGen(n)
    gen.setUniformParam()
    gen.condParser(checker).mkChart(gen.sentence)
  }

  "Unrestricted chart's incrementCounts" should "find an argmax tree and increment only parameteres used there (a->b)" in {

    val chart = mkChart(2, None, (0, 1))
    chart.incrementCounts()

    import LeftCornerSplitHeadNonterminals._

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

  it should "behave in the same way as the original chart" in {
    val chart = mkUniformChart(4, UnrestrictedLinkChecker)

    val originalChart = {
      val gen = new BiasedParserGen(4)
      gen.setUniformParam()
      gen.parser(0, UnlimitedDepthCalculator).mkChart(gen.sentence)
    }

    chart.incrementCounts()
    originalChart.incrementCounts()

    chart.marginal should be (originalChart.marginal)

    val param = chart.partial
    val oparam = originalChart.partial

    // param.root(0) should equal (oparam.root(0))
    // param.root(1) should equal (oparam.root(1))
    // param.root(2) should equal (oparam.root(2))

    // param.choice(0, Right)(1) should equal (oparam.choice(0, Right)(1))
    // param.choice(0, Right)(2) should equal (oparam.choice(0, Right)(2))
    // param.choice(1, Left)(0) should equal (oparam.choice(1, Left)(0))
    // param.choice(1, Right)(2) should equal (oparam.choice(1, Right)(2))
    // param.choice(2, Left)(0) should equal (oparam.choice(2, Left)(0))
    // param.choice(2, Left)(1) should equal (oparam.choice(2, Left)(1))

    // param.gen(0, Left, NoChild).stop should equal (1.0)
    // param.gen(0, Left, NoChild).proceed should equal (0.0)
    // param.gen(0, Left, HasChild).stop should equal (0.0)
    // param.gen(0, Left, HasChild).proceed should equal (0.0)

    // param.gen(0, Right, NoChild).stop should equal (1.0/2.0)
    // param.gen(0, Right, NoChild).proceed should equal (1.0/2.0)
    // param.gen(0, Right, HasChild).stop should equal (1.0/2.0)
    // param.gen(0, Right, HasChild).proceed should equal (0.0)

    // param.gen(1, Left, NoChild).stop should equal (1.0/2.0)
    // param.gen(1, Left, NoChild).proceed should equal (1.0/2.0)
    // param.gen(1, Left, HasChild).stop should equal (1.0/2.0)
    // param.gen(1, Left, HasChild).proceed should equal (0.0)

    // param.gen(1, Right, NoChild).stop should equal (0.0)
    // param.gen(1, Right, NoChild).proceed should equal (1.0)
    // param.gen(1, Right, HasChild).stop should equal (1.0)
    // param.gen(1, Right, HasChild).proceed should equal (0.0)

    // param.gen(2, Left, NoChild).stop should equal (1.0)
    // param.gen(2, Left, NoChild).proceed should equal (0)
    // param.gen(2, Left, HasChild).stop should equal (0)
    // param.gen(2, Left, HasChild).proceed should equal (0)

    // param.gen(2, Right, NoChild).stop should equal (1.0)
    // param.gen(2, Right, NoChild).proceed should equal (0.0)
    // param.gen(2, Right, HasChild).stop should equal (0.0)
    // param.gen(2, Right, HasChild).proceed should equal (0.0)

    equalParams(chart.partial, originalChart.partial)
  }

  "inside of root" should "be the same as outside of terminal" in {
    val chart = mkUniformChart(5, UnrestrictedLinkChecker)
    chart.terminalOutside(0) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(1) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(2) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(3) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(4) should be (2 * chart.marginal +- 1e-10)
  }

  "Restricted chart" should "find an argmax tree if condition is satisfied" in {

    val chart = mkChart(4, Some("a", "b"), (0, 1), (1, 2), (1, 3))
    chart.incrementCounts()

    import LeftCornerSplitHeadNonterminals._

    val param = chart.partial

    // // println("i: " + chart.insideScore(CondNonterm(RightFull)))
    // println("o: " + chart.outsideScore(CondNonterm(RightFullPred(1,1,2,0), true)))

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

    val chart = mkChart(4, Some("c", "a"), (0, 1), (1, 2), (1, 3))
    chart.marginal should be (0.0)

    chart.incrementCounts()

    val param = chart.partial

    val emptyParam = param.empty
    equalParams(param, emptyParam)
  }

  it should "behaves usually for one word sentence" in {

    val chart = mkChart(1, Some("a", "b"))
    chart.marginal should be (1.0)

    chart.incrementCounts()

    val param = chart.partial

    val manualParam = param.empty
    manualParam.incrementRoot(0, 1.0)
    manualParam.incrementStop(0, Left, NoChild, 1.0)
    manualParam.incrementStop(0, Right, NoChild, 1.0)

    equalParams(param, manualParam)
  }

  it should "decrease scores corresponding to unsatisfied trees (a->b)" in {

    val chart = mkChart(3, Some("a", "b"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (3.0)

    param.root(0) should equal (2.0/3.0)
    param.root(1) should equal (0.0)
    param.root(2) should equal (1.0/3.0)

    param.choice(0, Right)(1) should equal (1.0)
    param.choice(0, Right)(2) should equal (1.0/3.0)
    param.choice(1, Left)(0) should equal (0.0)
    param.choice(1, Right)(2) should equal (1.0/3.0)
    param.choice(2, Left)(0) should equal (1.0/3.0)
    param.choice(2, Left)(1) should equal (0.0)

    param.gen(0, Left, NoChild).stop should equal (3.0/3.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (0.0)
    param.gen(0, Right, NoChild).proceed should equal (1.0)
    param.gen(0, Right, HasChild).stop should equal (1.0)
    param.gen(0, Right, HasChild).proceed should equal (1.0/3.0)

    param.gen(1, Left, NoChild).stop should equal (1.0)
    param.gen(1, Left, NoChild).proceed should equal (0.0)
    param.gen(1, Left, HasChild).stop should equal (0.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (2.0/3.0)
    param.gen(1, Right, NoChild).proceed should equal (1.0/3.0)
    param.gen(1, Right, HasChild).stop should equal (1.0/3.0)
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

  it should "decrease scores corresponding to unsatisfied trees (a<-b)" in {

    val chart = mkChart(3, Some("b", "a"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (2.0)

    param.root(0) should equal (0.0)
    param.root(1) should equal (1.0/2.0)
    param.root(2) should equal (1.0/2.0)

    param.choice(0, Right)(1) should equal (0)
    param.choice(0, Right)(2) should equal (0)
    param.choice(1, Left)(0) should equal (1.0)
    param.choice(1, Right)(2) should equal (1.0/2.0)
    param.choice(2, Left)(0) should equal (0.0)
    param.choice(2, Left)(1) should equal (1.0/2.0)

    param.gen(0, Left, NoChild).stop should equal (3.0/3.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (1.0)
    param.gen(0, Right, NoChild).proceed should equal (0.0)
    param.gen(0, Right, HasChild).stop should equal (0.0)
    param.gen(0, Right, HasChild).proceed should equal (0.0)

    param.gen(1, Left, NoChild).stop should equal (0.0)
    param.gen(1, Left, NoChild).proceed should equal (1.0)
    param.gen(1, Left, HasChild).stop should equal (1.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (1.0/2.0)
    param.gen(1, Right, NoChild).proceed should equal (1.0/2.0)
    param.gen(1, Right, HasChild).stop should equal (1.0/2.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (1.0/2.0)
    param.gen(2, Left, NoChild).proceed should equal (1.0/2.0)
    param.gen(2, Left, HasChild).stop should equal (1.0/2.0)
    param.gen(2, Left, HasChild).proceed should equal (0.0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

  it should "decrease scores corresponding to unsatisfied trees (a->c)" in {

    val chart = mkChart(3, Some("a", "c"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (2.0)

    param.root(0) should equal (1.0)
    param.root(1) should equal (0.0)
    param.root(2) should equal (0.0)

    param.choice(0, Right)(1) should equal (1.0/2.0)
    param.choice(0, Right)(2) should equal (1.0)
    param.choice(1, Left)(0) should equal (0.0)
    param.choice(1, Right)(2) should equal (0.0)
    param.choice(2, Left)(0) should equal (0.0)
    param.choice(2, Left)(1) should equal (1.0/2.0)

    param.gen(0, Left, NoChild).stop should equal (1.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (0.0)
    param.gen(0, Right, NoChild).proceed should equal (1.0)
    param.gen(0, Right, HasChild).stop should equal (1.0)
    param.gen(0, Right, HasChild).proceed should equal (1.0/2.0)

    param.gen(1, Left, NoChild).stop should equal (1.0)
    param.gen(1, Left, NoChild).proceed should equal (0.0)
    param.gen(1, Left, HasChild).stop should equal (0.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (1.0)
    param.gen(1, Right, NoChild).proceed should equal (0.0)
    param.gen(1, Right, HasChild).stop should equal (0.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (1.0/2.0)
    param.gen(2, Left, NoChild).proceed should equal (1.0/2.0)
    param.gen(2, Left, HasChild).stop should equal (1.0/2.0)
    param.gen(2, Left, HasChild).proceed should equal (0.0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

  it should "decrease scores corresponding to unsatisfied trees (a<-c)" in {

    val chart = mkChart(3, Some("c", "a"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (2.0)

    param.root(0) should equal (0.0)
    param.root(1) should equal (0.0)
    param.root(2) should equal (1.0)

    param.choice(0, Right)(1) should equal (1.0/2.0)
    param.choice(0, Right)(2) should equal (0.0)
    param.choice(1, Left)(0) should equal (0.0)
    param.choice(1, Right)(2) should equal (0.0)
    param.choice(2, Left)(0) should equal (1.0)
    param.choice(2, Left)(1) should equal (1.0/2.0)

    param.gen(0, Left, NoChild).stop should equal (1.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (1.0/2.0)
    param.gen(0, Right, NoChild).proceed should equal (1.0/2.0)
    param.gen(0, Right, HasChild).stop should equal (1.0/2.0)
    param.gen(0, Right, HasChild).proceed should equal (0.0)

    param.gen(1, Left, NoChild).stop should equal (1.0)
    param.gen(1, Left, NoChild).proceed should equal (0.0)
    param.gen(1, Left, HasChild).stop should equal (0.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (1.0)
    param.gen(1, Right, NoChild).proceed should equal (0.0)
    param.gen(1, Right, HasChild).stop should equal (0.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (0.0)
    param.gen(2, Left, NoChild).proceed should equal (1.0)
    param.gen(2, Left, HasChild).stop should equal (1.0)
    param.gen(2, Left, HasChild).proceed should equal (1.0/2.0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

  it should "decrease scores corresponding to unsatisfied trees (b->c)" in {

    val chart = mkChart(3, Some("b", "c"))
    chart.paramHolder.param.initUniform

    chart.incrementCounts()

    val param = chart.partial

    chart.marginal should equal (2.0)

    param.root(0) should equal (1.0/2.0)
    param.root(1) should equal (1.0/2.0)
    param.root(2) should equal (0.0)

    param.choice(0, Right)(1) should equal (1.0/2.0)
    param.choice(0, Right)(2) should equal (0.0)
    param.choice(1, Left)(0) should equal (1.0/2.0)
    param.choice(1, Right)(2) should equal (1.0)
    param.choice(2, Left)(0) should equal (0.0)
    param.choice(2, Left)(1) should equal (0.0)

    param.gen(0, Left, NoChild).stop should equal (1.0)
    param.gen(0, Left, NoChild).proceed should equal (0.0)
    param.gen(0, Left, HasChild).stop should equal (0.0)
    param.gen(0, Left, HasChild).proceed should equal (0.0)

    param.gen(0, Right, NoChild).stop should equal (1.0/2.0)
    param.gen(0, Right, NoChild).proceed should equal (1.0/2.0)
    param.gen(0, Right, HasChild).stop should equal (1.0/2.0)
    param.gen(0, Right, HasChild).proceed should equal (0.0)

    param.gen(1, Left, NoChild).stop should equal (1.0/2.0)
    param.gen(1, Left, NoChild).proceed should equal (1.0/2.0)
    param.gen(1, Left, HasChild).stop should equal (1.0/2.0)
    param.gen(1, Left, HasChild).proceed should equal (0.0)

    param.gen(1, Right, NoChild).stop should equal (0.0)
    param.gen(1, Right, NoChild).proceed should equal (1.0)
    param.gen(1, Right, HasChild).stop should equal (1.0)
    param.gen(1, Right, HasChild).proceed should equal (0.0)

    param.gen(2, Left, NoChild).stop should equal (1.0)
    param.gen(2, Left, NoChild).proceed should equal (0)
    param.gen(2, Left, HasChild).stop should equal (0)
    param.gen(2, Left, HasChild).proceed should equal (0)

    param.gen(2, Right, NoChild).stop should equal (1.0)
    param.gen(2, Right, NoChild).proceed should equal (0.0)
    param.gen(2, Right, HasChild).stop should equal (0.0)
    param.gen(2, Right, HasChild).proceed should equal (0.0)
  }

}
