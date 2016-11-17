package lcdmv.model

import lcdmv.data.{DepArcs, Sentence}

trait ViterbiParser {
  def parse(sentence: Sentence): Option[(DepArcs, Double)]
}
