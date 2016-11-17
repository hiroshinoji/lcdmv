// package evalundep

// import jigg.util.IOUtil

// object RunEpic {
//   def main(args: Array[String]) = {
//     val sentences:IndexedSeq[IndexedSeq[String]] = IOUtil.openStandardIterator.toIndexedSeq.map(_.split("\t").toIndexedSeq)

//     val parser = epic.models.ParserSelector.loadParser("en").get

//     sentences foreach { sentence =>
//       val tree = parser(sentence)
//       println(tree)
//     }
//   }
// }
