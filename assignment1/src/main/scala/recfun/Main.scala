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

  type CoinCombo = Map[Int,Int] //denomination -> count
  def newCoinCombo(kv: (Int,Int)*) = Map(kv: _*)

  type Combinations = Set[CoinCombo]
  def newCombinations(xs: CoinCombo*) = Set(xs: _*)

  type Memo = Map[Int,Combinations]
  def newMemo(kv: (Int,Combinations)*) = Map(kv: _*)

  def countChange(money: Int, coins: List[Int]): Int = {
    val m = coins.foldLeft(newMemo())((memo, coin) =>
      memo + (coin -> newCombinations(newCoinCombo(coin -> 1).withDefaultValue(0))))
    countChangeHelper(m.withDefaultValue(newCombinations()), 1, money, coins)(money).size
  }

  @tailrec
  def countChangeHelper(acc: Memo, inc: Int, money: Int, coins: List[Int]): Memo = (acc, inc, money, coins) match {
    case (a, i, m, _) if(i > m) => a
    case (a, i, m, c) => {
      val result = coins.foldLeft(a(i))((combos, coin) =>
        combos.union(a(i - coin).map(coinCombo => coinCombo + (coin -> (coinCombo(coin) + 1))))
      )
      countChangeHelper((a + (i -> result)), i + 1, m, c)
    }


  }
}
