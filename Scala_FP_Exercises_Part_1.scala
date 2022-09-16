package com.techsophy

import scala.annotation.tailrec

object Scala_FP_Exercises_Part_1 {

  def main(args: Array[String]): Unit = {
    println(sumOfNumbersInList_V1(List(1,2,3,4,5)))
    println(sumOfNumbersInList_V2(List(1,2,3,4,5)))

    val listOfStrings = List("Hello World!", "How are You?", "Are you from Hyd?", "Are you working on Scala?")
    println(textProcessingTokenization(listOfStrings))
    textProcessingFilterSpecialChars(listOfStrings)
    println(textProcessingToLowerCase(listOfStrings))
    println(textProcessingTokenFrequency(listOfStrings))
    println(textProcessingMaxFrequencyToken(listOfStrings))
    println(textProcessingNormalizedFrequencyToken(listOfStrings))

  }


  // 1. Sum of all the numbers in the list
  def sumOfNumbersInList_V1(list: List[Int]): Int = list.sum

  def sumOfNumbersInList_V2(list: List[Int]): Int = {
    @tailrec
    def sumLoop(list: List[Int], acc: Int): Int = {
      list match {
        // list is represented as head :: tail. Last element is Nil.
        case Nil => acc
        case head :: tail => sumLoop(tail, head + acc)
      }
    }
    sumLoop(list, 0)
  }

  // a. Tokenisation - Split strings by a whitespace character - it will generate another string.
  def textProcessingTokenization(strList: List[String]): List[String] = {
    strList.flatMap(s => s.split(" "))
  }

  // b. Filter out any token containing special symbols.
  def textProcessingFilterSpecialChars(strList: List[String]): Any = {
    val pattern = "\\W".r
    val spclChars = strList.flatMap(s => s.split(" "))
      .map(x => (pattern findAllIn x.trim).mkString(""))
      .foreach(println)

    println(spclChars)
  }

  // c. Lower case every token.
  def textProcessingToLowerCase(strList: List[String]): List[String] = {
    strList.flatMap(s => s.split(" "))
      .map(x => x.toLowerCase)
  }

  // d. Find frequency of every token and store them in a Map.
  def textProcessingTokenFrequency(strList: List[String]): Any = {
    strList.flatMap(s => s.split(" "))
      .map(x => x.toLowerCase)
      .groupBy(x => x)
      .view.mapValues(x => x.length)
      .toMap
  }

  // e. Find max frequency token.
  def textProcessingMaxFrequencyToken(strList: List[String]): (String, Int) = {
    val processedMap = strList.flatMap(s => s.split(" "))
      .map(x => x.toLowerCase)
      .groupBy(x => x)
      .view.mapValues(x => x.length)
      .toMap

    processedMap.toSeq.sorted.head
  }

  // f. For the Map generated in (4), divide their frequency by max value.
  def textProcessingNormalizedFrequencyToken(strList: List[String]): List[Float] = {
    val processedMap = strList.flatMap(s => s.split(" "))
      .map(x => x.toLowerCase)
      .groupBy(x => x)
      .view.mapValues(x => x.length)
      .toMap

    val maxFreq = processedMap.toSeq.sorted.head._2
    processedMap.map(x => x._2.toFloat/maxFreq).toList
  }


  /**
   * 1 - done
   * a - done
   * b - done
   * c - done
   * d -
   * e -
   * f -
   */

}
