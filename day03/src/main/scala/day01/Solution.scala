package day01

import scala.util.Try

object Solution {
  
  def move(visitedHouses: List[(Int, Int)], instruction: Char): List[(Int, Int)] = {
    val current = visitedHouses.head
    val next = instruction match {
      case '^' => (current._1, current._2 - 1) 
      case 'v' => (current._1, current._2 + 1)
      case '<' => (current._1 - 1, current._2)
      case '>' => (current._1 + 1, current._2)
    }
    visitedHouses.filter(_ == next).headOption match {
      case Some(preexistingNext) => 
        next :: visitedHouses.filterNot(_ == preexistingNext) 
      case None => next :: visitedHouses
    }
  }
  
  def housesThatReceivedAtLeastOnePresent(moves: List[Char]): Int = {
    moves.foldLeft(List[(Int, Int)]((0,0)))((acc, elm) => move(acc, elm)).size
  }
  
//  FIXME: This does not seem to be the best solution. Think about it.
  def housesThatReceivedAtLeastOnePresentWithRobotHelp(moves: List[Char]): Int = {
    val santaMoves = moves.sliding(1,2).toList.flatten
    val robotMoves = Try(moves.tail.sliding(1,2).toList.flatten).getOrElse(Nil)    
    val s = santaMoves.foldLeft(List[(Int, Int)]((0,0)))((acc, elm) => move(acc, elm))
    val r = robotMoves.foldLeft(List[(Int, Int)]((0,0)))((acc, elm) => move(acc, elm))
    ((s ::: r) toSet) size    
  }
}