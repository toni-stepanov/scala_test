import scala.collection.mutable.ListBuffer

object FirstWeek {


  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def main(args: Array[String]): Unit = {
    println(countChange(4, List(2, 1)))
    //    val bool = balance(List('c', '(', '(', 't', ')', 'g', ')', '(', ')', ')', '('))
    //    println("__________ Second : " + bool)
    //    println("__________ Pascal")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row) {
    //        print(pascal(col, row) + " ")
    //      }
    //      println(" ")
    //    }
  }


  def balance(chars: List[Char]): Boolean = {
    def checkBalance(chars: List[Char], numOpens: Int): Boolean = {
      if (chars.isEmpty) {
        numOpens == 0
      } else {
        val head = chars.head
        val num =
          if (head == '(') numOpens + 1
          else if (head == ')') numOpens - 1
          else numOpens
        if (numOpens >= 0) checkBalance(chars.tail, num)
        else false
      }
    }

    checkBalance(chars, 0)
  }

  def countChange2(money: Int, coins: List[Int]): Int = {
    def f(lastMaxCoin_total_coll: List[(Int, Int)], count: Int): Int = {
      if (lastMaxCoin_total_coll.isEmpty) {
        count
      } else {
        val b = ListBuffer[(Int, Int)]()
        var newCount = count
        for ((lastMaxCoin, total) <- lastMaxCoin_total_coll) {
          if (total < money) {
            for (c <- coins) {
              if (c >= lastMaxCoin) {
                val e = (c, total + c)
                b += e
              }
            }
          } else if (total == money) {
            newCount += 1
          }
        }
        f(b.toList, newCount)
      }
    }

    val b = coins.map { c => (c, c) }
    f(b, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
      def count(money: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0
        else if (money == coins.head) 1
        else if (money < coins.head) 0
        else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    count(money, coins.sorted)
  }

}
