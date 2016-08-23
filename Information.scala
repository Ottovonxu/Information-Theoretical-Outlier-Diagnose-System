package org.apache.spark.examples.MyCode



/**
 * Created by otto on 8/1/16.
 */




object Information {



  def main(args: Array[String]) {

    //val intMatrix = Array.ofDim[String](6,1)
    val myentropy=new shannonEntropy()
    println(myentropy.shannonEntropy("aaaaaaaaaab"))
    val counts1 : List[String]= List("2.2","3.3","a","a","aa","b")
    val result=counts1.groupBy(w => w)
      .map(kv => kv)
    println(result)


  }

}
