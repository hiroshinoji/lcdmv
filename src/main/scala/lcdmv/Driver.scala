package lcdmv

import fig.exec.Execution

object Driver {
  def main(args:Array[String]) = {
    val runner = new Runner
    Execution.run(args, runner, "driver", DriverOptions,
      "input", InputOptions,
      "output", OutputOptions,
      "train", TrainingOptions,
      "parser", ParserOptions,
      "reguralize", ReguralizeOptions)
  }
}

class Runner extends Runnable {
  import OptionEnumTypes._

  def run = {
    val problem = DriverOptions.actionType match {
      case ActionType.train => new Train
      case ActionType.featureTrain => new FeatureTrain
      case ActionType.evaluate => new Evaluate
      case ActionType.coverage => new CalcCoverage
      case ActionType.likelihood => new Likelihood
      case _ => sys.error("unimplemented")
    }
    problem.run
  }
}
