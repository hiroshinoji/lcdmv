package evalundep

object POSTag {
  // val puncs = Set("!", "#", "$", "''", "(", ")", ",", "-LRB-", "-RRB-", ".", ":", "?", "``")
  val puncs = Set("!", "''", "(", ")", ",", "-LRB-", "-RRB-", ".", ":", "?", "``")
  val verbs = Set("VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "MD")
  val nouns = Set("NN", "NNS", "NNP", "NNPS", "PRP", "WP")

  def isPunc(pos: String) = puncs.contains(pos)
  def isVerb(pos: String) = verbs.contains(pos)
  def isNoun(pos: String) = nouns.contains(pos)
  def isPrep(pos: String) = pos == "IN" || pos == "TO"
}
