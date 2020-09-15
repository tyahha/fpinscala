package fpinscala

object FibonacciNumber {
  // exercise 2.1
  def fib(n: Int): Int = {
    if (n <= 0) {
      0
    } else if (n == 1) {
      1
    } else {
      @annotation.tailrec
      def go(curIndex: Int, prev: Int, preprev: Int): Int = {
        val cur = prev + preprev
        if (curIndex == n) {
          prev + preprev
        } else {
          go(curIndex + 1, cur, prev)
        }
      }
      go(2, 1, 0)
    }
  }
}
