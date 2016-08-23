package org.apache.spark.examples.MyCode




import org.apache.spark.{SparkContext, SparkConf}


import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object ReadCSV1 {
  val conf = new SparkConf().setAppName("test").setMaster("local[2]")
  val sc = new SparkContext(conf)
  class reader(){
    def read(path:String):scala.io.BufferedSource=Source.fromFile(path)

    def data(s:scala.io.BufferedSource)={
      val ObjectArray=new ArrayBuffer[String] ()
      //val attributes=new ArrayBuffer[String] ()
      //val len=s.length
      //ObjectArray+= len.toString
      for(line <- s.getLines){
        val temp=Array[String](line)
        val cols = line.split(",").map(_.trim)
        //println(cols.length)
        ObjectArray++=cols
        val len=cols.length
        ObjectArray+=len.toString
      }
      ObjectArray

    }
    def run(path:String)=data(read(path))
  }

  def main(args: Array[String]): Unit = {
    val read = new reader()
    val path = "/home/otto/Downloads/Archive/2005.csv"
    val CSVdata=read.run(path)
    val product=CSVdata.length
    val numattri=CSVdata(product-1).toInt
    val numobj=product/(numattri+1)
    println("Number of Attributes:")
    println(numattri)
    println("Number of Objects:")
    println(numobj)
    val attributes=new Array[String] (numattri*numobj)
    for (j<-0 until numobj){
      for (i<-0 until numattri){
        attributes(i*numobj+j)=CSVdata(i+j*(numattri+1))
      }
    }

    println(attributes(0))
    println(attributes(1))
    println(attributes(2))
    println(attributes.length)

  }

}
