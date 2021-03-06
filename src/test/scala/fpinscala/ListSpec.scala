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
    "sum2" - {
      "1,2,3" in {
        assert(List.sum2(List(1,2,3)) == 6)
      }
    }
    "product2" - {
      "1,2,3" in {
        assert(List.product2(List(1,2,3)) == 6)
      }
    }
    "sum3" - {
      "1,2,3" in {
        assert(List.sum3(List(1,2,3)) == 6)
      }
    }
    "product3" - {
      "1,2,3" in {
        assert(List.product3(List(1,2,3)) == 6)
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
      assert(List() == List())
      assert(List(1) == List(1))
      assert(List(1,2) == List(1,2))
      assert(List(1,2,3) == List(1,2,3))
    }

    "unmatch" in {
      assert(List() != List(1))
      assert(List(1) != List())
      assert(List(1) != List(1,2))
      assert(List(1,2,3) != List(1,2))
      assert(List(1,2,3) != List(1,2,4))
    }
  }

  "tail" in {
    assert(List.tail(List()) == Nil)
    assert(List.tail(List(1)) == Nil)
    assert(List.tail(List(1,2)) == List(2))
    assert(List.tail(List(1,2,3)) == List(2,3))
  }

  "setHead" in {
    assert(List.setHead(List[Int](), 1) == Nil)
    assert(List.setHead(List(2), 1) == List(1))
    assert(List.setHead(List(2, 3), 1) == List(1, 3))
  }

  "drop" in {
    assert(List.drop(List(), 100) == Nil)
    assert(List.drop(List(1), 0) == List(1))
    assert(List.drop(List(1), 1) == Nil)
    assert(List.drop(List(1, 2), 1) == List(2))
    assert(List.drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  "dropWhile" in {
    assert(List.dropWhile(List[Int]())(_ > 100) == Nil)
    assert(List.dropWhile(List(1,2,3,4,5))(_ <= 3) == List(4,5))
  }

  "init" in {
    assert(List.init(Nil) == Nil)
    assert(List.init(List(1)) == Nil)
    assert(List.init(List(1, 2)) == List(1))
    assert(List.init(List(1, 2, 3)) == List(1, 2))
  }

  // exercise 3.8
  "foldRight from list to list" in {
    val got = List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))
    assert(got == List(1,2,3))
  }

  "length" in {
    assert(List.length(Nil) == 0)
    assert(List.length(List(1)) == 1)
    assert(List.length(List(1, 2)) == 2)
  }

  "reverse" in {
    assert(List.reverse(Nil) == Nil)
    assert(List.reverse(List(1)) == List(1))
    assert(List.reverse(List(1,2)) == List(2,1))
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
  }

  "append2" in {
    assert(List.append(Nil, Nil) == Nil)
    assert(List.append(Nil, List(1)) == List(1))
    assert(List.append(List(1), Nil) == List(1))
    assert(List.append(List(1), List(1)) == List(1, 1))
    assert(List.append(List(1,2), List(1)) == List(1, 2, 1))
    assert(List.append(List(1,2), List(1,2)) == List(1, 2, 1, 2))
  }

  "flatten" in {
    assert(List.flatten(List(List(1,2), List(3,4))) == List(1,2,3,4))
  }

  "add1" in {
    assert(List.add1(List(1,2,3)) == List(2,3,4))
  }

  "doublesToString" in {
    assert(List.doublesToString(List(1.0,2.0,3.0)) == List("1.0","2.0","3.0"))
  }

  "filter" in {
    assert(List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))
  }
  "filter2" in {
    assert(List.filter2(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))
  }
  "hasSubsequence" in {
    assert(List.hasSubsequence(List(1,2,3,4), List(2,3)))
    assert(!List.hasSubsequence(List(1,2,3,4), List(2,4)))
  }
}
