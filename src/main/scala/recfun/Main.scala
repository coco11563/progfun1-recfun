package recfun

import java.util

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10){
      print(" " * (10 - row))
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(just an) example".toList))

    println(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
  val cache = new util.HashMap[(Int, Int), Int]
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) return 1
    if (r == 0) return 1
    if (r == c) return 1
    if (cache.containsKey((c, r))) cache.get((c, r))
    else{
      val value = pascal(c, r - 1) + pascal(c - 1, r - 1)
      cache.put((c, r), value)
      value
    }
  }
    val store = new util.Stack[Char]
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {
      if (chars.isEmpty && store.isEmpty) return true
      if (chars.isEmpty && !store.isEmpty) return false
      val c : Char = chars.head
      if (c != '(' && c != ')') balance(chars.tail)
      if (store.isEmpty) {
        if (c == ')') return false
        else {
          store.push(c)
          return balance(chars.tail)
        }
      }
      if (c != store.peek()) {
        store.pop()
        balance(chars.tail)
      } else {
        store.push(c)
        balance(chars.tail)
      }
    }
  val cacheCoin = new util.HashMap[Int, Int]
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) return 1
      if (money < 0) return 0
      if (coins.isEmpty) return 0
      if (cacheCoin.containsKey(money)) cacheCoin.get(money)
      val c = countChange(money - coins.head, coins) +
        //        countChange(money - coins.head, coins.tail) +
        countChange(money, coins.tail)
      cacheCoin.put(money, c)
      c
    }

  def sum(f:Int => Int, a: Int , b :Int) :Int = {
    def loop(a : Int, acc : Int) : Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }
}
