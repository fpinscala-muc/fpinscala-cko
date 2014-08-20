package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("no list given")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("no list given")
    case Cons(_, t) => Cons(h, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) {
      l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
    } else {
      l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case Cons(h, t) => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("no list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => append(List(h), init(t))
  }

  def lengthSimple[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, t) => 1 + length(t)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, len) => len + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumViaFoldRight(ints: List[Int]): Int = {
    foldRight(ints, 0)((a, b) => a + b)
  }

  //use double as return value
  def sumViaFoldRight2(ints: List[Int]): Double = {
    foldRight(ints, 0.0)((a, b) => a + b)
  }

  // use wildcards
  def sumViaFoldRight3(ints: List[Int]): Double = {
    foldRight(ints, 0.0)(_ + _)
  }

  def sumViaFoldLeft(nums: List[Int]): Int = {
    foldLeft(nums, 0)(_ + _)
  }

  def productViaFoldLeft(nums: List[Double]): Double = {
    foldLeft(nums, 1.0)((_ * _))
  }

  def lengthViaFoldLeft(l: List[_]): Int = {
    foldLeft(l, 0)((a, b) => 1 + a)
  }

  def reverseSimple[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), Cons(h, Nil))
  }
  
   def reverse[A](l: List[A]): List[A] =
    	foldLeft(l, Nil:List[A])((t,h) => Cons(h,t))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((a, b) => Cons(b, a))
  }

  def concat[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(list, xs) => append(list, concat(xs))
  }

  def add1Simple(nums: List[Int]): List[Int] = nums match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  def add1[T](nums: List[T])(implicit ev: Numeric[T]): List[T] = 
    foldRight(nums, Nil:List[T])((a, list) => Cons(ev.plus(a, ev.one), list))
  
  def doubleToString(l: List[Double]): List[String] = 
    foldLeft(reverse(l), Nil:List[String])((a, b) => Cons(b.toString(), a))
    

  def map[A, B](l: List[A])(f: A => B): List[B] = 
	  foldLeft(reverse(l), Nil:List[B])((a,b) => Cons(f(b), a))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldLeft(reverse(l), Nil:List[A])((a,b) => if (!f(b)){a} else{Cons(b, a)})

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = 
    foldLeft(reverse(l), Nil:List[B])((a,b)=> append(f(b), a))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)((a) => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(h,t)) => Nil
    case (Cons(h,t), Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons (h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(h,t)) => Nil
    case (Cons(h,t), Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons (f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l,sub) match {
    case (Nil, _) => false
    case (l, Nil) => true
    case (Cons(h1,t1), Cons(h2,Nil)) => 
      	if(h1 == h2) {true} else {hasSubsequence(t1, sub)} 
    case (Cons(h1,t1), Cons(h2,t2)) => 
      	if(h1 == h2) {hasSubsequence(t1, t2)} 
      	else {hasSubsequence(t1, sub)}
  }
}