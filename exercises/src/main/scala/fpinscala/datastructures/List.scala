package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2(l: List[_]): Int =
    foldLeft(l, 0)( (acc, _) => acc + 1)

  private def swap[A,B, C](f: (A,B) => C): (B, A) => C = { (b,a) => f(a,b) }
  def append1[A](l: List[A], item: A): List[A] = l match {
    case Nil => Cons(item, Nil)
    case Cons(hd, tl) => Cons(hd, append1(tl, item))
  }
  def reverse1[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])(swap(append1[A]))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse1(l), z)(swap(f))

  def reverse2[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])(swap(Cons.apply[A]))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse2(as), z)(swap(f))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons.apply)




  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, tl) => Cons( h, tl)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, tl) => drop(tl, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, tl) if f(h) => dropWhile(tl, f)
    case x => x
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, tl) => Cons(h, init(tl))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)( (_, b) => 1 + b)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, tl) => foldLeft(tl, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, tl) => Cons(f(h), map(tl)(f))
  }

  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  def addOne(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  def doubleString(l: List[Double]): List[String] =
    map(l)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) { (el, acc) =>
      if (f(el)) Cons(el, acc)
      else acc
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B]) { (acc, el) =>
      append(acc, f(el))
    }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(a, tla), Cons(b, tlb)) => Cons(f(a, b), zipWith(tla, tlb)(f))
    case _ => Nil
  }

  def addTwoLists(as: List[Int], bs: List[Int]): List[Int] = zipWith(as, bs)(_ + _)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def samePrefix(sup1: List[A], sub1: List[A]): Option[Boolean] = (sup1, sub1) match {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) if hd1 == hd2 => samePrefix(tl1, tl2)
      case (_, Nil) => Some(true)
      case (Nil, _) => None
      case _ => Some(false)
    }

    samePrefix(sup, sub).fold(false)(
      _ || (sub match {
        case Cons(_, tl) => hasSubsequence(tl, sub)
        case _ => false
      })
    )
  }

}

object ListTest extends App {
  import List._
  println(reverse1(List(1,2,3,4,5)))
  println(foldLeft2(List(1,2,3), "")(_ + _.toString))

  println(reverse2(List(1,2,3,4,5)))
  println(foldRight2(List(1,2,3), "")(_.toString + _))

  println(append2(List(1,2,4), List(9,5)))

  println(concat(List(List(1,2,3), List(4,5), List(6,7,8,9), List(10))))
  println(filter2(concat(List(List(1,2,3), List(4,5), List(6,7,8,9), List(10))))(_ % 3 == 0))
  println(
    addTwoLists(List(1,2,3), List(4,5,6))
  )

}
