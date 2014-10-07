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
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Stream.empty
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(h,t), n) if n > 0 => Some(h(), (t(), n-1))
      case _ => None
    }
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

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile(p))
    case _ => Stream.empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h,t) if p(h()) => Some (h(), t())
      case _ => None
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => (p(a) && b))
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else Stream.empty)
  }

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h,t) => cons(f(h()), t().map(f))
    case _ => Stream.empty
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)
  }

  def append[B>:A](other: Stream[B]): Stream[B] = {
    foldRight(other)((a,b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Cons(h,t) => f(h()) append( t().flatMap(f))
    case _ => Stream.empty
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this,s2)){
      case (Cons(h1,t1),Cons(h2,t2)) => Some( f(h1(),h2()), (t1(),t2()))
      case _ => None
    }
  }
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

  def from(n: Int): Stream[Int] = {
      Stream.cons(n, from(n+1))
    }

  val fibs: Stream[Int] = {
     def go(n0: Int, n1: Int): Stream[Int] = 
      Stream.cons(n0, go(n1, n0+n1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => Stream.empty
  }

  val fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)){ case (n0, n1) => Some (n0,(n1,n0 + n1))}
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(n => Some (n, n+1))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some (a, a))
  }

  val onesViaUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1,1))
  }
}