package org.apache.spark.examples.MyCode

/**
 * Created by otto on 8/3/16.
 */
class shannonEntropy {

  def shannonEntropy(str: String): Double = {
    val counts = str.groupBy(c => c)
      .map(kv => kv._2.length)

    val len: Double = str.length
    (for {
      count <- counts
      iter = (count / len) * scala.math.log(count/len) / scala.math.log(2)
    } yield iter)
      .sum * (-1)
  }
}
