package fpinscala.laziness

import Stream._
import fpinscala.laziness.Algebras._
import fpinscala.laziness.StreamF._
import fpinscala.laziness.Transducers._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldWithTransducer[B, (B,Stream[B]), A, A](scan)(z)(f)._2

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(hd, tl) if n > 0 => cons(hd(), tl().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case s if n == 0 => s
    case Cons(_, tl) => tl().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])( (a, st) => if (p(a)) cons(a, st) else empty[A] )

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])( (a, b) => cons(f(a), b))

  def map2[B](f: A => B): Stream[B] =
    runWithTransducer(Transducers.map[A, B, Stream[B]](f))

  def map3[B](f: A => B): Stream[B] =
    runWithCoTransducer(CoTransducers.map[A, B, Stream[A]](f))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])( (a, acc) => if (p(a)) cons(a, acc) else acc)

  def filter2(p: A => Boolean): Stream[A] =
    runWithTransducer(Transducers.filter[A, Stream[A]](p))

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, st) =>
      f(a).append(st)
    }

  def flatMap2[B](f: A => Stream[B]): Stream[B] =
    runWithTransducer(Transducers.flatMap[A, B, Stream[B]](f))
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll {
      case (l, r) => l == r
    }


  def tails: Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]])(_.flatMap {
      case s@Cons(_, t) => Some((s, Some(t())))
      case _ => Some((empty[A], None))
    })

  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(hd, tl) => hd() :: tl().toList()
  }

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case _ => None
    }
  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(hd, tl), i) if i > 0 => Some((hd(), (tl(), i - 1)))
      case _ => None
    }
  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(hd, tl) if p(hd()) => Some((hd(), tl()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (_, Cons(h2,t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }



  def foldWithTransducer[B, D, AA >: A, X](
    transducer: Transducer[StreamF[X, *], B, StreamF[AA, *], D]
  )(z: => B)(f: (X, => B) => B): D = {
    val alg = transducer {
      case EmptyF => z
      case ConsF(h, tl) => f(h(), tl())
    }
    foldRight[D](alg(emptyF))((h, t) => alg(consF(h, t)))
  }

  def runWithTransducer[D, AA >: A, X](
    transducer: Transducer[StreamF[X, *], Stream[X], StreamF[AA, *], D]
  ): D = foldWithTransducer(transducer)(empty[X])( (a,b) => cons(a, b))

  def runWithCoTransducer[B, AA >: A, X](
    coTransducer: CoTransducer[StreamF[X, *], Stream[X], StreamF[B, *], Stream[AA]]
  ): Stream[B] =
    unfoldWithCoTransducer[Stream[X], Stream[AA], B, X](coTransducer)(this) {
      case Empty => None
      case Cons(h, t) => Some((h(), t()))
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait StreamF[+A, +B]
case object EmptyF extends StreamF[Nothing, Nothing]
case class ConsF[+A, +B](h: () => A, t: () => B) extends StreamF[A, B]
object Algebras {
  type Algebra[F[_], A] = F[A] => A
  type CoAlgebra[F[_], A] = A => F[A]
  type Transducer[F[_], A, G[_], B] = Algebra[F,A] => Algebra[G, B]
  type CoTransducer[F[_], A, G[_], B] = CoAlgebra[F,A] => CoAlgebra[G, B]
}

object StreamF {
  def consF[A, B](hd: => A, tl: => B): StreamF[A, B] = {
    lazy val head = hd
    lazy val tail = tl
    ConsF(() => head, () => tail)
  }

  def emptyF[A, B]: StreamF[A, B] = EmptyF
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  val fibs: Stream[Int] = {
    def go(`n - 1`: Int, n: Int): Stream[Int] =
      cons(`n - 1`, go(n, n + `n - 1`))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def unfoldWithCoTransducer[B, D, A, X](
    coTransducer: CoTransducer[StreamF[X, *], B, StreamF[A, *], D]
  )(z: D)(f: B => Option[(X, B)]): Stream[A] = {
    val coAlgebra: CoAlgebra[StreamF[A, *], D] = coTransducer { b =>
      f(b).fold(emptyF[X,B]) { case (x, b) => consF(x, b) }
    }

    unfold(z)(coAlgebra.andThen {
      case EmptyF => None
      case ConsF(a, d) => Some((a(), d()))
    })
  }


  val fibsUnfold: Stream[Int] = unfold((0,1)) {
    case (n0, n1) => Some((n0, (n1, n0 + n1)))
  }

  def fromUnfold(n: Int): Stream[Int] = unfold(n){
    i => Some((i, i  + 1))
  }
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))
  val onesUnfold: Stream[Int] = constantUnfold(1)

}

object Transducers {
  def scan[B, A]: Transducer[StreamF[A, *], B, StreamF[A, *], (B, Stream[B])] = { xf =>
  {
    case EmptyF =>
      val b: B = xf(emptyF)
      (b, Stream(b))
    case ConsF(hd, tl) =>
      lazy val t = tl()
      lazy val b: B = xf(consF(hd(), t._1))
      (b, cons(b, t._2))
  }
  }

  def map[A, B, C](f: A => B): Transducer[StreamF[B, *], C, StreamF[A, *], C] = { xf => {
    case EmptyF => xf(emptyF)
    case ConsF(h, t) => xf(consF(f(h()), t()))
  }
  }

  def filter[A, C](f: A => Boolean): Transducer[StreamF[A, *], C, StreamF[A, *], C] = { xf => {
    case EmptyF => xf(emptyF)
    case ConsF(h, t) =>
      val hd = h()
      if (f(hd)) xf(consF(hd, t())) else t()
  }
  }

  def flatMap[A, B, C](f: A => Stream[B]): Transducer[StreamF[B, *], C, StreamF[A, *], C] = { xf => {
    case EmptyF => xf(emptyF)
    case ConsF(h, t) =>
      f(h()).foldRight[C](t())((hd, tl) => xf(consF(hd, tl)))
  }
  }
}

object CoTransducers {

  def map[A,B,C](f: A => B): CoTransducer[StreamF[A, *], C, StreamF[B, *], C] = {cxf => c =>
    cxf(c) match {
      case EmptyF => EmptyF
      case ConsF(h, t) => consF(f(h()), t())
    }
  }

}

object test extends App {
  println(fibsUnfold.take(10).toList())

  println(Stream(1,2,3,4).scanRight(0)(_ + _).toList())

  println(Stream(1,2,3).map2(5.*).filter2(_ % 10 != 0).toList())
  println(Stream(1,2,3).map3(5.*).filter2(_ % 10 != 0).toList())

  println(
    Stream(1,2,3)
      .runWithTransducer(Transducers.map[Int, Int, Stream[Int]](5 * _).compose(Transducers.filter(_ % 10 != 0))
      )
      .flatMap2(x => Stream.constant(x).take(x))
      .toList()
  )
}
