package lcdmv

object Utils {
  def filledArray(n: Int)(v: Double) = Array.fill(n)(v)
  def filledArray2(n1: Int, n2: Int)(v: Double) = Array.fill(n1)(filledArray(n2)(v))
  def filledArray3(n1: Int, n2: Int, n3: Int)(v: Double) = Array.fill(n1)(filledArray2(n2, n3)(v))
  def filledArray4(n1: Int, n2: Int, n3: Int, n4: Int)(v: Double) = Array.fill(n1)(filledArray3(n2, n3, n4)(v))
  def filledArray5(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(v: Double) = Array.fill(n1)(filledArray4(n2, n3, n4, n5)(v))
  def filledArray6(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, n6: Int)(v: Double) = Array.fill(n1)(filledArray5(n2, n3, n4, n5, n6)(v))

  def enumerate[E](map: Array[Array[E]]): Iterator[(List[Int], E)] =
    for (i <- (0 until map.size).iterator;
      (lst, e) <- enumerate(map(i))
    ) yield (i :: lst, e)

  def enumerate[E](array: Array[E]): Iterator[(List[Int], E)] =
    for (i <- (0 until array.size).iterator) yield (i :: Nil, array(i))

  // def foreachAll[E](map: Array[Array[E]])(f: => Unit) =
  //   map foreach { foreachAll(_)(f) }
  // def foreachAll[E](array: Array[E])(f: => Unit) =

  def normalize(array: Array[Double]): Unit = {
    val sum = array.sum
    (0 until array.size) foreach { i => array(i) /= sum }
  }
}
