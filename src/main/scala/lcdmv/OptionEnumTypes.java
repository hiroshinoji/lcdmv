package lcdmv;

public class OptionEnumTypes {
  public enum Format { CoNLLX, PascalCoNLL, twoline };
  public enum ActionType { train, featureTrain, evaluate, predict, analyze, coverage, likelihood };
  public enum InputToken { surface, pos };
  public enum Posdef { pos, upos, cpos };
  public enum TrainMethod { em, viterbi };
  public enum Initializer { uniform, random, harmonic, supervise };
  public enum HowToReadAtTrain { allTrainAndTest, activeTrainAndTest, allTrain, activeTrain };
  public enum UnkConverter { simple, surfaceFeature };
  public enum TokenModifier { numberCrusher, lowerCaser };

  public enum LinkPOSConstraint { none, functionLeave };
  public enum RootPOSConstraint { none, verb, verbAndNoun, verbOrNoun, verbNounAdj };

  public enum SatisfiedLink { none, verbNoun };
  public enum UnleafPOS { none, adp, adpPrtConj };
  public enum UnheadPOS { none, udfunc, detConj, detConjPrt, detPrt };

  public enum ChunkDef { basic, content }
  
  // public enum DistanceCostType { none, hard, exp };
}
