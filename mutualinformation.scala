package org.apache.spark.examples.MyCode

import scala.collection.mutable.ArrayBuffer
import scala.math
/**
 * Created by otto on 8/4/16.
 */
class mutualinformation {
  def combinestr(Attri1:Array[String],Attri2:Array[String])={

    val combineattri=new Array[String](Attri1.length)

    for(i<-0 until Attri1.length) {
      combineattri(i) = Attri1(i).concat(Attri2(i))
    }
    combineattri
  }
  def mutualinformation(Attri1:Array[String],Attri2:Array[String])={
    val Attri=combinestr(Attri1,Attri2)
    val combineentropy=new entr()
    val jointentropy=combineentropy.shannonEntr(Attri)
    val entropy1=combineentropy.shannonEntr(Attri1)
    val entropy2=combineentropy.shannonEntr(Attri2)
    val MI=entropy1+entropy2-jointentropy
    MI
  }
  def ISF(Attri1:Array[String],Attri2:Array[String])={
    val Attri=combinestr(Attri1,Attri2)
    val combineentropy=new entr()
    val jointentropy=combineentropy.shannonEntr(Attri)
    val entropy1=combineentropy.shannonEntr(Attri1)
    val entropy2=combineentropy.shannonEntr(Attri2)
    val MI=entropy1+entropy2-jointentropy
    val isf=MI+entropy1+entropy2
    isf
  }
  def Choose2attributes(Num1:Int,Num2:Int,Dataset:Array[String],objectnumber:Int,attributesnumber:Int)= {
    if((Num1<attributesnumber)&&(Num2<attributesnumber)){
      val attriselect1=new Array[String](objectnumber-1)
      val attriselect2=new Array[String](objectnumber-1)
      for(k<-1 until objectnumber){
        attriselect1(k-1)=Dataset(Num1*objectnumber+k)
        attriselect2(k-1)=Dataset(Num2*objectnumber+k)
      }
      ISF(attriselect1,attriselect2)
    }
    else println("Wrong in number")
  }
  def findMISS(Dataset:Array[String],objectnumber:Int,attributesnumber:Int)={
    val AllISF=new ArrayBuffer[Double]()
    for(i<-0 until attributesnumber){
      for(j<-i+1 until attributesnumber){
        val temp=Choose2attributes(i,j,Dataset,objectnumber,attributesnumber)
        val value=temp.asInstanceOf[Double]
        if(value<0.06){
          AllISF+=i
          AllISF+=j
          AllISF+=value
        }

      }
    }
    AllISF

  }
  def MISSnumber(ISFcombination:ArrayBuffer[Double],attributesnumber:Int) ={
    val length=ISFcombination.length
    val MISSlabel=new ArrayBuffer[Double]()
    val MISSlength=MISSlabel.length
    val flag=0
    for(i<-0 until length){
      if(i%3!=2){
        MISSlabel+=ISFcombination(i)
        }
      }
    val MISSsorted=MISSlabel.sorted.toArray
    val MISSlist=MISSlabel.toList.distinct
    val MISSleft=MISSlist.toArray.toBuffer
    for (i<-0 until attributesnumber){
      if(MISSlist.exists(n=>n==i)==false){
        MISSleft+=i.toDouble
      }
    }
    MISSleft.toArray
    }
  def Extractsubsets(Dataset:Array[String],Extractnumber: Array[Double],objectnumber:Int,attributesnumber:Int)={
    val Subsets=new ArrayBuffer[String]()
    for(i<-0 until Extractnumber.length){
      for(j<-0 until objectnumber){
        val temp=Extractnumber(i).toInt
        Subsets+=Dataset(objectnumber*temp+j)
      }
    }
    Subsets
  }
  def CalDiffHoloentropy(Dataset:Array[String],objectnumber:Int,attributesnumber:Int)={
    val reciprocal_of_b=objectnumber-1
    val reciprocal_of_a=reciprocal_of_b-1
    val coef1=(-1)*scala.math.log(reciprocal_of_a)+(reciprocal_of_b/reciprocal_of_a)*scala.math.log(reciprocal_of_b)
    val entropy_each_attributes=new Array[Double](attributesnumber)
    val attributeselect=new Array[String](objectnumber-1)
    val calculateentropy=new entr()
    val OFfactor=new Array[Double](objectnumber-1)
    val labelmatrix=new Array[String]((objectnumber-1)*attributesnumber)

    for (i<-0 until attributesnumber){
      for (k <- 1 until objectnumber) {
        attributeselect(k - 1) = Dataset(i* objectnumber + k)
      }
      entropy_each_attributes(i)=calculateentropy.shannonEntr(attributeselect.toArray)
      val counts = attributeselect.groupBy(w => w).mapValues(_.size).toList
      val frequency=new ArrayBuffer[String]()
      for(i1<-0 until counts.length){
        frequency+=counts(i1)._1
        if(counts(i1)._2==1){
          frequency+=0.toString
        }
        else frequency+=((counts(i1)._2-1)*scala.math.log((counts(i1)._2-1))-(counts(i1)._2)*scala.math.log((counts(i1)._2))).toString
      }
      for(i2<-0 until attributeselect.length){
        for(j<-0 until frequency.length by 2){
          if(attributeselect(i2)==frequency(j)){
            OFfactor(i2)=frequency(j+1).toDouble
            attributeselect(i2)=(coef1*reciprocal_of_a-entropy_each_attributes(i)+frequency(j+1).toDouble).toString
            if(attributeselect(i2).toDouble>0){
              labelmatrix(i*(objectnumber-1)+i2)=1.toString
            }
            else labelmatrix(i*(objectnumber-1)+i2)=0.toString
          }
        }
      }
      val minimumOF=OFfactor.min
      for(i2<-0 until attributeselect.length){

        for(i2<-0 until attributeselect.length){
          if(OFfactor(i2)==0||OFfactor==minimumOF){
            labelmatrix(i*(objectnumber-1)+i2)=1.toString
          }
        }
      }

    }
    labelmatrix






  }
  }




