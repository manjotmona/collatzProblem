package edu.knoldus

import scala.collection.mutable.ListBuffer
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

  def producingListThroughCollatzEquation(element: Int, list: ListBuffer[Int]): ListBuffer[Int] = {
    if (element == 1) {
      if (!(isElementPresent(list.toList, 1))) {
        list += 1
      }
      list
    }
    else if (isElementPresent(list.toList, element)) {
      list
    }
    else if (element % 2 == 0) {
      list += element
      val newElement = element / 2
      producingListThroughCollatzEquation(newElement, list)
    }
    else {
      list += element
      val newElement = 3 * element + 1
      producingListThroughCollatzEquation(newElement, list)
    }
  }
}

object ApplyCollatzSolution extends App {
  val log = Logger.getLogger(this.getClass)
  log.info("Hello, here is the Collatz Problem!!\n\n")
  val sol = new SolutionOfCollatzProblem()
  val list = new ListBuffer[Int]
  val hundred = 100
  val nineteen = 19
  val seven = 7
  sol.producingListThroughCollatzEquation(hundred, list)
  log.info(list + "\n")
  sol.producingListThroughCollatzEquation(nineteen, list)
  log.info(list + "\n")
  sol.producingListThroughCollatzEquation(seven, list)
  log.info(list + "\n")
}
