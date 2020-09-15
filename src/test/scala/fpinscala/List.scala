package fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  @annotation.tailrec
  def equal[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (Nil, Nil) => true
    case (Cons(_, _), Nil) => false
    case (Nil, Cons(_, _)) => false
    case (Cons(ae, at), Cons(be, bt)) =>
      if (ae == be) {
        equal(at, bt)
      } else {
        false
      }
  }

  // exercise 3.3
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }
}