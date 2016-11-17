package lcdmv.model

import org.scalatest._

class NoConstraintSplitHeadParserSpec extends FlatSpec with Matchers with SplitHeadParserSetting {

  def mkChart(n: Int, activeLinks: (Int, Int)*) = {
    val gen = new BiasedParserGen(n)
    gen.setBiasedParam(activeLinks:_*)
    gen.parser(0, UnlimitedDepthCalculator).mkChart(gen.sentence)
  }
  def mkUniformChart(n: Int) = {
    val gen = new BiasedParserGen(n)
    gen.setUniformParam()
    gen.parser(0, UnlimitedDepthCalculator).mkChart(gen.sentence)
  }

  "Chart's incrementCounts" should "find an argmax tree and increment only parameteres used there (a->b)" in {

    val chart = mkChart(2, (0, 1))
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
    val chart = mkChart(3, (0, 1), (0, 2))
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

  it should "do the same thing with another parameterization (a->c; b<-c)" in {
    val chart = mkChart(3, (0, 2), (2, 1))
    chart.incrementCounts()

    val param = chart.partial

    param.root(0) should equal (one)
    param.choice(0, Right)(2) should equal (one)
    param.choice(2, Left)(1) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).proceed should equal (one)
    param.gen(0, Right, HasChild).stop should equal (one)
    param.gen(1, Left, NoChild).stop should equal (one)
    param.gen(1, Right, NoChild).stop should equal (one)
    param.gen(2, Left, NoChild).proceed should equal (one)
    param.gen(2, Left, HasChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  it should "do the same thing with another parameterization (a->b; b->c)" in {
    val chart = mkChart(3, (0, 1), (1, 2))
    chart.incrementCounts()

    val param = chart.partial

    param.root(0) should equal (one)
    param.choice(0, Right)(1) should equal (one)
    param.choice(1, Right)(2) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).proceed should equal (one)
    param.gen(0, Right, HasChild).stop should equal (one)
    param.gen(1, Left, NoChild).stop should equal (one)
    param.gen(1, Right, NoChild).proceed should equal (one)
    param.gen(1, Right, HasChild).stop should equal (one)
    param.gen(2, Left, NoChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  it should "do the same thing with another parameterization (a<-b; b->c)" in {
    val chart = mkChart(3, (1, 0), (1, 2))
    chart.incrementCounts()

    val param = chart.partial

    param.root(1) should equal (one)
    param.root(0) should equal (0)
    param.choice(1, Left)(0) should equal (one)
    param.choice(1, Right)(2) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).stop should equal (one)
    param.gen(1, Left, NoChild).proceed should equal (one)
    param.gen(1, Left, HasChild).stop should equal (one)
    param.gen(1, Right, NoChild).proceed should equal (one)
    param.gen(1, Right, HasChild).stop should equal (one)
    param.gen(2, Left, NoChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  it should "do the same thing with another parameterization (a<-c; b<-c)" in {
    val chart = mkChart(3, (2, 0), (2, 1))
    chart.incrementCounts()

    val param = chart.partial

    param.root(2) should equal (one)
    param.choice(2, Left)(0) should equal (one)
    param.choice(2, Left)(1) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).stop should equal (one)
    param.gen(1, Left, NoChild).stop should equal (one)
    param.gen(1, Right, NoChild).stop should equal (one)
    param.gen(2, Left, NoChild).proceed should equal (one)
    param.gen(2, Left, HasChild).proceed should equal (one)
    param.gen(2, Left, HasChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  it should "do the same thing with another parameterization (a<-c; a->b)" in {
    val chart = mkChart(3, (2, 0), (0, 1))
    chart.incrementCounts()

    val param = chart.partial

    param.root(2) should equal (one)
    param.choice(2, Left)(0) should equal (one)
    param.choice(0, Right)(1) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).proceed should equal (one)
    param.gen(0, Right, HasChild).stop should equal (one)
    param.gen(1, Left, NoChild).stop should equal (one)
    param.gen(1, Right, NoChild).stop should equal (one)
    param.gen(2, Left, NoChild).proceed should equal (one)
    param.gen(2, Left, HasChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  it should "do the same thing with another parameterization (a<-b; b<-c)" in {
    val chart = mkChart(3, (1, 0), (2, 1))
    chart.incrementCounts()

    val param = chart.partial

    param.root(2) should equal (one)
    param.choice(2, Left)(1) should equal (one)
    param.choice(1, Left)(0) should equal (one)

    param.gen(0, Left, NoChild).stop should equal (one)
    param.gen(0, Right, NoChild).stop should equal (one)
    param.gen(1, Left, NoChild).proceed should equal (one)
    param.gen(1, Left, HasChild).stop should equal (one)
    param.gen(1, Right, NoChild).stop should equal (one)
    param.gen(2, Left, NoChild).proceed should equal (one)
    param.gen(2, Left, HasChild).stop should equal (one)
    param.gen(2, Right, NoChild).stop should equal (one)
  }

  "inside of root" should "be the same as outside of terminal" in {
    val chart = mkUniformChart(5)
    chart.terminalOutside(0) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(1) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(2) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(3) should be (2 * chart.marginal +- 1e-10)
    chart.terminalOutside(4) should be (2 * chart.marginal +- 1e-10)

    // chart.incrementCounts()

    // val partial = chart.partial

    // import java.io.PrintWriter
    // val w = new PrintWriter(System.out)
    // partial.writeTo(w, _ + "")
    // w.flush()
    // w.close()
  }

}
