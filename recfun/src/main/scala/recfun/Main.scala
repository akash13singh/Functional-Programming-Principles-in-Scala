package recfun

import javax.naming.InsufficientResourcesException

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
  def pascal(c: Int, r: Int): Int =
    if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

  def f( chars: List[Char],sum: Int): Boolean = {
    if (chars.isEmpty && sum == 0) true
    else if (sum < 0) false
    else if (chars.head == '(') f(chars.tail, sum + 1)
    else if (chars.head == ')') f(chars.tail, sum - 1)
    else f(chars.tail, sum)
    }
    f(chars.toList,0)
  }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 0
  }
