package recfun

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
    def pascal(c: Int, r: Int): Int = {
      val returnVal =
        if (c == 0 || c == r ) {
          1
        }
        else {
          pascal(c-1, r-1) + pascal(c, r-1)
        }
      returnVal
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = this.parenthesisCount(0, chars)

    def parenthesisCount(count:Int ,chars: List[Char]): Boolean = {

      val valToReturn =
      if (chars.isEmpty) {
        if (count == 0) true else false
      }
      else{
        val head = chars.head
        val thisCount = if (head == '(') 1 else if (head == ')') -1 else 0
        val newCount = count + thisCount
        if (newCount < 0 ) false else parenthesisCount(newCount, chars.tail)
      }
      valToReturn
    }
  /*
  *
  */

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      val ways =
        if (money == 0){
          1
        }
        else if (money <0 || coins.isEmpty){
          0
        }
      else{
          countChange(money, coins.tail) + countChange(money-coins.head, coins)
        }
      ways
    }
  }
