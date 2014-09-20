package fpinscala.laziness

import Stream._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  
  def toList: List[A] = {
    @tailrec
    def loop (a: Stream[A], b: List[A]):List[A] = a match {
      case Cons(h,t) => loop(t(), h() :: b)
      case _ => b
    } 
    loop(this, List.empty).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n<= 0 => Stream.empty
    case Cons(h,t) if n == 1 =>  cons(h(), Stream.empty)
    case Cons(h,t) => cons(h(), t().take(n-1))
    case _ => Stream.empty
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    sys.error("todo")
  }

  def drop(n: Int): Stream[A] = {
    if (n > 0) {
      this match {
        case Cons(h, t) => t().drop(n - 1)
        case _ => Stream.empty
      }
    } else {
      this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = sys.error("todo")

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => (p(a) && b))
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = sys.error("todo")

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def map[B](f: A => B): Stream[B] = sys.error("todo")

  def mapViaUnfold[B](f: A => B): Stream[B] = sys.error("todo")

  def filter(p: A => Boolean): Stream[A] = sys.error("todo")

  def append[B>:A](other: Stream[B]): Stream[B] = {
    foldRight(other)((a,b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = sys.error("todo")

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = sys.error("todo")
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = sys.error("todo")

  val fibs: Stream[Int] = {
      Stream.empty
    }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => Stream.empty
  }

  val fibsViaUnfold: Stream[Int] = {
    Stream.empty
  }

  def fromViaUnfold(n: Int): Stream[Int] = sys.error("todo")

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some (a, a))
  }

  val onesViaUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1,1))
  }
}