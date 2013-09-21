package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = pascalHelper(List(1), c, r)

  @tailrec
  def pascalHelper(a: List[Int], c: Int, r:Int): Int = (a, c, r) match {
    case (acc, x, y) if (acc.length - 1) == y => a(x)
    case (acc, x, y) => pascalHelper((1 :: acc.iterator.sliding(2,1).withPartial(false).map((l: Seq[Int]) => l.sum).toList) :+ 1, x, y)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balanceHelper(0, chars)

  @tailrec
  def balanceHelper(acc: Int, chars: List[Char]): Boolean = (acc, chars) match {
    case (-1, _) => false
    case (a, c) if(c.isEmpty) => 0 == a
    case (a, '(' :: rest) => balanceHelper(a + 1, rest)
    case (a, ')' :: rest) => balanceHelper(a - 1, rest)
    case (a, _ :: rest) => balanceHelper(a, rest)
  }



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 0
    case (_, c) if(c.isEmpty) => 0
    case (m, c :: rest) => 1 + countChange(m - c, rest)

  }

  //@tailrec
  def countChangeHelper(acc: Map[Int,Int], money: Int, coins: List[Int]): Int = ??? /*(acc, money, coins) match {
    case (a, m, _) if(a.contains(money)) => a(money)
    case (a, m, c) 
  }*/
}
