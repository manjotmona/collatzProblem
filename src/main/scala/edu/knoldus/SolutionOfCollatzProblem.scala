package edu.knoldus

import org.apache.log4j.Logger

/**
 * Created by manjot on 14/1/18.
 */
class SolutionOfCollatzProblem {

  def isElementPresent(list: List[Int], element: Int): Boolean = {
    list match {
      case Nil => false
      case head :: tail if (head == element) => true
      case head :: tail => isElementPresent(tail, element)
    }
  }

  def producingListThroughCollatzEquation(element: Int,
      list: List[Int],
      tList: List[Int]): List[Int] = {
    if (element == 1) {
      if (!(isElementPresent(list, 1))) {
        1 :: list ::: tList
      }
      else {
        list ::: tList
      }
    }
    else if (isElementPresent(list, element)) {
      list ::: tList
    }
    else if (element % 2 == 0) {
      val newList = element :: tList
      val newElement = element / 2
      producingListThroughCollatzEquation(newElement, list, newList)
    }
    else {
      val newList = element :: tList
      val newElement = 3 * element + 1
      producingListThroughCollatzEquation(newElement, list, newList)
    }
  }
}

object ApplyCollatzSolution extends App {
  val log = Logger.getLogger(this.getClass)
  log.info("Hello, here is the Collatz Problem!!\n\n")
  val sol = new SolutionOfCollatzProblem()
  val list = List()
  val hundred = 100
  val nineteen = 19
  val seven = 7
  val newList1 = sol.producingListThroughCollatzEquation(hundred, list, list)
  log.info(newList1 + "\n")
  val newList2 = sol.producingListThroughCollatzEquation(nineteen, newList1, list)
  log.info(newList2 + "\n")
  val newList3 = sol.producingListThroughCollatzEquation(seven, newList2, list)
  log.info(newList3 + "\n")
}
