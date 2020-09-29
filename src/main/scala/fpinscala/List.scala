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

  // exercise 3.13
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (z: B) => z)((a, g) => b => g(f(b, a)))(z)

  // exercise 3.14
  def append2[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)(Cons(_, _))

  // exercise 3.15
  def flatten[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil: List[A])(append)

  // exercise 3.16
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // exercise 3.17
  def doublesToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  // exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, l) => if (f(a)) Cons(a, l) else l)

  // exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // exercise 3.22
  def addEach(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, aa), Cons(b, bb)) => Cons(a + b, addEach(aa, bb))
  }

  // exercise 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, aa), Cons(b, bb)) => Cons(f(a,b), zipWith(aa, bb)(f))
  }

  // exercise 3.34
  @annotation.tailrec
  def startWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 =>startWith(t1, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }
}