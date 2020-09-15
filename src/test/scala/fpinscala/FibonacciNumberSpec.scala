package fpinscala

import org.scalatest._

class FibonacciNumberSpec extends FreeSpec {
  "fibonacci number" - {
    "0" in {
      assert(FibonacciNumber.fib(0) == 0)
    }
    "1" in {
      assert(FibonacciNumber.fib(1) == 1)
    }
    "2" in {
      assert(FibonacciNumber.fib(2) == 1)
    }
    "3" in {
      assert(FibonacciNumber.fib(3) == 2)
    }
    "4" in {
      assert(FibonacciNumber.fib(4) == 3)
    }
    "5" in {
      assert(FibonacciNumber.fib(5) == 5)
    }
    "6" in {
      assert(FibonacciNumber.fib(6) == 8)
    }
  }
}
