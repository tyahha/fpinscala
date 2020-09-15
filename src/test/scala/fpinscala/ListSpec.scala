package fpinscala

import org.scalatest.FreeSpec

class ListSpec extends FreeSpec {
  "List" - {
    "sum" - {
      "1,2,3" in {
        assert(List.sum(List(1,2,3)) == 6)
      }
    }
    "product" - {
      "1,2,3" in {
        assert(List.product(List(1,2,3)) == 6)
      }
    }
  }

  // exercise 3.1
  "pattern matching" in {
    val x = List(1,2,3,4,5,6) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // match this
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  "equal" - {
    "match" in {
      assert(List.equal(List(), List()))
      assert(List.equal(List(1), List(1)))
      assert(List.equal(List(1,2), List(1,2)))
      assert(List.equal(List(1,2,3), List(1,2,3)))
    }

    "unmatch" in {
      assert(!List.equal(List(), List(1)))
      assert(!List.equal(List(1), List()))
      assert(!List.equal(List(1), List(1,2)))
      assert(!List.equal(List(1,2,3), List(1,2)))
      assert(!List.equal(List(1,2,3), List(1,2,4)))
    }
  }

  "tail" in {
    assert(List.equal(List.tail(List()), Nil))
    assert(List.equal(List.tail(List(1)), Nil))
    assert(List.equal(List.tail(List(1,2)), List(2)))
    assert(List.equal(List.tail(List(1,2,3)), List(2,3)))
  }

  "setHead" in {
    assert(List.equal(List.setHead(List[Int](), 1), Nil))
    assert(List.equal(List.setHead(List(2), 1), List(1)))
    assert(List.equal(List.setHead(List(2, 3), 1), List(1, 3)))
  }
}
