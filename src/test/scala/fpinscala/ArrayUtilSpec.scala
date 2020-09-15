package fpinscala

import org.scalatest._

class ArrayUtilSpec extends FreeSpec {
  "ArrayUtil" - {
    "isSorted" - {
      "1 2 3" in {
        assert(ArrayUtil.isSorted(Array(1,2,3), (a: Int, b: Int) => a < b))
      }
      "3 2 1" in {
        assert(!ArrayUtil.isSorted(Array(3,2,1), (a: Int, b: Int) => a < b))
      }
      "1 3 2" in {
        assert(!ArrayUtil.isSorted(Array(1,3,2), (a: Int, b: Int) => a < b))
      }
      "1" in {
        assert(ArrayUtil.isSorted(Array(1), (a: Int, b: Int) => a < b))
      }
      "empty" in {
        assert(ArrayUtil.isSorted(Array.empty[Int], (a: Int, b: Int) => a < b))
      }
    }
  }
}
