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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)
  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  // exercise 3.11
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def length(as: List[_]): Int = foldLeft(as, 0)((len, _) => len + 1)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }


  // exercise 3.3
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  // exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(a, l) if f(a) => dropWhile(l)(f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((as, a) => Cons(a, as))

}