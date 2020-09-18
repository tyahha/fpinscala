package fpinscala

object ArrayUtil {
  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n > as.length - 2) {
        true
      } else {
        if (ordered(as(n), as(n + 1))) {
          loop(n + 1)
        } else {
          false
        }
      }
    }

    loop(0)
  }
}
