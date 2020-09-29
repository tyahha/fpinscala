package fpinscala

import org.scalatest.FreeSpec

class TreeSpec extends FreeSpec {
  "Tree" - {
    "size" in {
      assert(Tree.size(Leaf(1)) == 1)
      assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
      assert(Tree.size(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 5)
    }
    "maximum" in {
      assert(Tree.maximum(Leaf(1)) == 1)
      assert(Tree.maximum(Branch(Leaf(1), Leaf(2))) == 2)
      assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 3)
    }
    "depth" in {
      assert(Tree.depth(Leaf(1)) == 0)
      assert(Tree.depth(Branch(Leaf(1), Leaf(2))) == 1)
      assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 2)
    }
    "map" in {
      assert(Tree.map(Leaf(1))(_.toString) == Leaf("1"))
    }
  }
}
