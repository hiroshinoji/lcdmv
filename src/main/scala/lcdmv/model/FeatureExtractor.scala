package lcdmv

import const.Condition.{GenCondition, ChoiceCondition}
import data.{PoS}
import scala.collection.mutable.ArrayBuffer

trait FeatureExtractor[C] {
  def extract(context: C, features: ArrayBuffer[String]): Unit
}

trait GenFeatureExtractor extends FeatureExtractor[GenCondition]

// useful functions to implement features from Berg-kirkpatrick et al.'s paper
trait PainlessExtractor {
  // def isNoun(pos: PoS) = pos.upos == "NOUN" || pos.upos == "PRON" // pos(0) == 'N' || pos == "PRP" || pos == "WP"
  // def isVerb(pos: PoS) = pos.isVerb == "VERB" // pos(0) == 'V'|| pos == "MD"

  def boolStr(b: Boolean) = if (b) '1' else '0'
}

class PainlessGenExtractor(pos: Int=>PoS) extends GenFeatureExtractor with PainlessExtractor {

  def extract(context: GenCondition, features: ArrayBuffer[String]) = context match {
    case GenCondition(h, d, v, dec) =>
      val head = pos(h)
      val dir = d.toString
      val valence = v.toString
      val decision = dec.toString

      // basic
      features += s"gen_d=${decision}_h=${head}_dir=${dir}_v=${valence}"
      features += s"gen_d=${decision}_h=${head}_dir=${dir}"
      features += s"gen_d=${decision}_h=${head}_v=${valence}"
      features += s"gen_d=${decision}_h=${head}" // these features do not originally exist

      // noun
      if (head.isNoun) {
        features += s"gen_d=${decision}_noun_h_dir=${dir}_v=${valence}"
        features += s"gen_d=${decision}_noun_h_dir=${dir}"
        features += s"gen_d=${decision}_noun_h_v=${valence}"
        features += s"gen_d=${decision}_noun_h"
      }

      // verb
      if (head.isVerb) {
        features += s"gen_d=${decision}_verb_h_dir=${dir}_v=${valence}"
        features += s"gen_d=${decision}_verb_h_dir=${dir}"
        features += s"gen_d=${decision}_verb_h_v=${valence}"
        features += s"gen_d=${decision}_verb_h"
      }

      // noun or verb
      if (head.isNoun || head.isVerb) {
        features += s"gen_d=${decision}_noun_verb_h_dir=${dir}_v=${valence}"
        features += s"gen_d=${decision}_noun_verb_h_dir=${dir}"
        features += s"gen_d=${decision}_noun_verb_h_v=${valence}"
        features += s"gen_d=${decision}_noun_verb_h"
      }
  }
}

trait ChoiceFeatureExtractor extends FeatureExtractor[ChoiceCondition]

class PainlessChoiceExtractor(pos: Int=>PoS) extends ChoiceFeatureExtractor with PainlessExtractor {

  def extract(context: ChoiceCondition, features: ArrayBuffer[String]) = context match {
    case ChoiceCondition(h, d, a) =>
      val head = pos(h)
      val dir = d.toString
      val arg = pos(a)

      // basic
      features += s"choice_a=${arg}_h=${head}_dir=${dir}"
      features += s"choice_a=${arg}_h=${head}"
      // features += s"choice_h=${head}"

      if (arg.isNoun) {
        features += s"choice_noun_a_h=${head}_dir=${dir}"
        features += s"choice_noun_a_h=${head}"
      }
      if (arg.isVerb) {
        features += s"choice_verb_a_h=${head}_dir=${dir}"
        features += s"choice_verb_a_h=${head}"
      }

      if (arg.isNoun || arg.isVerb) {
        features += s"choice_noun_verb_a_h=${head}_dir=${dir}"
        features += s"choice_noun_verb_a_h=${head}"
      }

      // head=noun
      if (head.isNoun) {
        features += s"choice_a=${arg}_noun_h_dir=${dir}"
        features += s"choice_a=${arg}_noun_h"
        // features += s"choice_noun_h" // offset for head choice (originally not exist)

        if (arg.isNoun) {
          features += s"choice_noun_a_noun_h_dir=${dir}"
          features += s"choice_noun_a_noun_h"
        }
        if (arg.isVerb) {
          features += s"choice_verb_a_noun_h_dir=${dir}"
          features += s"choice_verb_a_noun_h"
        }
        if (arg.isNoun || arg.isVerb) {
          features += s"choice_noun_verb_a_noun_h_dir=${dir}"
          features += s"choice_noun_verb_a_noun_h"
        }
      }

      // head=verb
      if (head.isVerb) {
        features += s"choice_a=${arg}_verb_h_dir=${dir}"
        features += s"choice_a=${arg}_verb_h"
        // features += s"choice_verb_h"

        if (arg.isNoun) {
          features += s"choice_noun_a_verb_h_dir=${dir}"
          features += s"choice_noun_a_verb_h"
        }
        if (arg.isVerb) {
          features += s"choice_verb_a_verb_h_dir=${dir}"
          features += s"choice_verb_a_verb_h"
        }
        if (arg.isNoun || arg.isVerb) {
          features += s"choice_noun_verb_a_verb_h_dir=${dir}"
          features += s"choice_noun_verb_a_verb_h"
        }
      }
      if (head.isNoun || head.isVerb) {
        features += s"choice_a=${arg}_noun_verb_h_dir=${dir}"
        features += s"choice_a=${arg}_noun_verb_h"
        // features += s"choice_noun_verb_h"

        if (arg.isNoun) {
          features += s"choice_noun_a_noun_verb_h_dir=${dir}"
          features += s"choice_noun_a_noun_verb_h"
        }
        if (arg.isVerb) {
          features += s"choice_verb_a_noun_verb_h_dir=${dir}"
          features += s"choice_verb_a_noun_verb_h"
        }
        if (arg.isNoun || arg.isVerb) {
          features += s"choice_noun_verb_a_noun_verb_h_dir=${dir}"
          features += s"choice_noun_verb_a_noun_verb_h"
        }
      }
  }
}

class FeatureExtractors[C](val lst: Seq[FeatureExtractor[C]]) {
  def extract(context: C): Array[String] = {
    val features = new ArrayBuffer[String]
    lst foreach { extractor =>
      extractor.extract(context, features)
    }
    features.toArray
  }
}
