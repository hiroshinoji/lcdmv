package lcdmv.learn

import collection.parallel.ForkJoinTaskSupport
import concurrent.forkjoin.ForkJoinPool

package object learn {

  def makePar[Datum](data: IndexedSeq[Datum], nThreads: Int)(indices: IndexedSeq[Int]) = {
    val xx = indices.par.map(data)
    if (nThreads > 0) xx.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nThreads))
    xx
  }

  def parGroups[Datum](data: IndexedSeq[Datum], k: Int, numThreads: Int) = {
    val groups = data.grouped(data.size / k).toIndexedSeq
    makePar(groups, numThreads)(0 until groups.size)
  }

}
