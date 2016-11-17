package lcdmv

import data._
import java.io._
import scala.collection.mutable.ArrayBuffer

case class ReadCondition(n: Int, minLen: Int, maxLen: Int) {
  def reduce[S<:SentenceLike](sentences: IndexedSeq[S]): IndexedSeq[S] = {
    val satisfyMaxLength = if (maxLen > 0) sentences.filter(_.size <= maxLen) else sentences
    val satisfyMinLength = if (minLen > 0) satisfyMaxLength.filter(_.size >= minLen) else satisfyMaxLength
    if (n > 0) satisfyMinLength.take(n) else satisfyMinLength
  }
}

trait CorpusReader[S] {
  def readSentences(path: String): IndexedSeq[S]
}

class TwoLineReader(cond: ReadCondition = ReadCondition(0, 0, 0)) extends CorpusReader[StringSentence] {
  def readSentences(path: String): IndexedSeq[StringSentence] = {
    val sentences = new ArrayBuffer[StringSentence]
    var sentence = new Array[String](2)

    def addSentence = {
      val word = sentence(0).split(" ")
      val pos = sentence(1).split(" ")
      sentences += StringSentence(word, pos)
    }
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(path)))
    Iterator.continually(reader.readLine()).takeWhile(_!=null).zipWithIndex.foreach {
      case (line, i) if i % 3 == 2 =>
        assert(line == "")
        addSentence
        sentence(0) = ""
      case (line, i) if i % 3 == 0 => sentence(0) = line
      case (line, i) if i % 3 == 1 => sentence(1) = line
    }
    if (sentence(0) != "") addSentence

    cond.reduce(sentences)
  }
}

trait CoNLLReader extends CorpusReader[CoNLLSentence] {
  def cond: ReadCondition

  def readSentences(path: String): IndexedSeq[CoNLLSentence] = readSentences(Seq(path))

  def readSentences(paths: Seq[String]): IndexedSeq[CoNLLSentence] = {
    val sentences = new ArrayBuffer[CoNLLSentence]

    paths foreach { path =>
      var sentence = new ArrayBuffer[String]
      val reader = new BufferedReader(new InputStreamReader(new FileInputStream(path)))
      Iterator.continually(reader.readLine()).takeWhile(_!=null).foreach {
        case "" if !sentence.isEmpty =>
          sentences += toCoNLLSentence(sentence)
          sentence.clear
        case "" =>
        case line => sentence += line
      }
      if (!sentence.isEmpty) sentences += toCoNLLSentence(sentence)
    }
    cond.reduce(sentences)
  }

  def toCoNLLSentence(lines: IndexedSeq[String]): CoNLLSentence
}

class CoNLLXReader(val cond: ReadCondition = ReadCondition(0, 0, 0)) extends CoNLLReader {
  def toCoNLLSentence(lines: IndexedSeq[String]): CoNLLSentence = CoNLLXSentence.fromLines(lines)
}

class PascalCoNLLReader(val cond: ReadCondition = ReadCondition(0, 0, 0)) extends CoNLLReader {
  def toCoNLLSentence(lines: IndexedSeq[String]): CoNLLSentence = CoNLLXSentence.fromUniversalLines(lines)
}
