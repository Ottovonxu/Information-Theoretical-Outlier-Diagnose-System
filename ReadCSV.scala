package org.apache.spark.examples.MyCode

import org.apache.spark.{SparkContext, SparkConf}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.collection.mutable

object ReadCSV {
  val conf = new SparkConf().setAppName("test").setMaster("local[2]")
  val sc = new SparkContext(conf)

  class reader() {
    def read(path: String): scala.io.BufferedSource = Source.fromFile(path)

    def data(s: scala.io.BufferedSource) = {
      val ObjectArray = new ArrayBuffer[String]()
      //val attributes=new ArrayBuffer[String] ()
      //val len=s.length
      //ObjectArray+= len.toString
      for (line <- s.getLines) {
        val temp = Array[String](line)
        val cols = line.split(",").map(_.trim)
        //println(cols.length)
        ObjectArray ++= cols
        val len = cols.length
        ObjectArray += len.toString
      }
      //ObjectArray+= len.toString
      ObjectArray

    }

    def run(path: String) = data(read(path))
  }

  def main(args: Array[String]): Unit = {
    val read = new reader()
    val path = "/home/otto/Downloads/Archive/2005.csv"
    val CSVdata = read.run(path)
    val product = CSVdata.length
    val numattri = CSVdata(product - 1).toInt
    val numobj = product / (numattri + 1)
    println("Number of Attributes:")
    println(numattri)
    println("Number of Objects:")
    println(numobj)
    val attributes = new Array[String](numattri * numobj)
    for (j <- 0 until numobj) {
      for (i <- 0 until numattri) {
        attributes(i * numobj + j) = CSVdata(i + j * (numattri + 1))
      }
    }
    val attriselect = new Array[String](numobj - 1)

    for (k <- 1 until numobj) {
      attriselect(k - 1) = attributes(23 * numobj + k)
    }

    val myentropy = new entr()
    println(myentropy.shannonEntr(attriselect))
    val myMI = new mutualinformation()
    val combine1 = myMI.combinestr(attriselect, attriselect)
    val MI = myMI.mutualinformation(attriselect, attriselect)
    val isf = myMI.ISF(attriselect, attriselect)
    println(MI)
    println(isf)
    val cal = myMI.Choose2attributes(23, 23, attributes, numobj, numattri)
    println(cal)
    val Misscombination = myMI.findMISS(attributes, numobj, numattri)
    /**for (i <- 0 until Misscombination.length) {
      println(Misscombination(i))
    }**/
    println("next")

    val Miss =myMI.MISSnumber(Misscombination,numattri)
    for (i <- 0 until Miss.length) {
      println(Miss(i)+1)
    }
    val Subsets=myMI.Extractsubsets(attributes,Miss,numobj,numattri)
    /**for (i <- 0 until Subsets.length) {
      println(Subsets(i))
    }**/
    val counts = attriselect.groupBy(w => w).mapValues(_.size).toList
    val frequency=new ArrayBuffer[String]()
    for(i<-0 until counts.length){
      frequency+=counts(i)._1
      if(counts(i)._2==1){
        frequency+=0.toString
      }
      else frequency+=((counts(i)._2-1)*scala.math.log((counts(i)._2-1))-(counts(i)._2)*scala.math.log((counts(i)._2))).toString
    }
    println(frequency)
    for(i<-0 until attriselect.length){
      for(j<-0 until frequency.length by 2){
        if(attriselect(i)==frequency(j)){
          attriselect(i)=frequency(j+1)
        }
      }
      //println(attriselect(i))
    }
    val sparsematrix=myMI.CalDiffHoloentropy(attributes,numobj,numattri)
    for (i<-0 until sparsematrix.length){
      println(sparsematrix(i))
    }
    val counts1 = sparsematrix.groupBy(w => w).mapValues(_.size).toList
    println(counts1)






  }
}

