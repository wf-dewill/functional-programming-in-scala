import scala.annotation.tailrec

object Chapter3 extends App{

  /* Ex. 3.1 */
//  val x = List(1,2,3,4,5) match {
//    case Cons(x, Cons(2, Cons(4, _))) => x
//    case Nil => 42
//    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
//    case Cons(h, t) => h + sum(t)
//    case _ => 101
//  } === 3
//  Not 1 because third element doesn't match
//  Not 2 because the list is defined, not Nil
//  3 is hit, because the pattern matches, gives 3

  /* Ex. 3.2 */
  def tail[A](lst: List[A]): List[A] = lst match {
    case _::rest => rest
    case Nil => Nil
  }

  /* Ex. 3.3 */
  def setHead[A](lst: List[A], newHead: A): List[A] = lst match {
    case _::xs => newHead::xs
    case Nil => List(newHead)
  }

  /* Ex. 3.4 */
  @tailrec def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case _::xs => drop(xs, n-1)
      case Nil => Nil
  }
  }

  /* Ex. 3.5 */
  @tailrec def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
    l match {
      case x::xs => if (p(x)) dropWhile(xs, p) else l
      case Nil => Nil
    }

  /* Ex. 3.6 */
  def init[A](l: List[A]): List[A] = {
    @tailrec def elemGetter(lst: List[A], holder: List[A]): List[A] = {
      lst match {
        case x::xs => if (xs.isEmpty) holder else elemGetter(xs, holder++List(x))
        case Nil => Nil
      }
    }
    elemGetter(l, List())
  }

  /* Ex. 3.7 */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x::xs => f(x, foldRight(xs, z)(f))
  }
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
  // Something like if ns.contains(0) then 0 else recursion
  // Depends how well contains is implemented whether it would
  // be faster to just do recursion, but for a big list might just be
  // faster to do recursion rather than searching

  /* Ex. 3.8 */
  foldRight[Int, List[Int]](List(1,2,3), Nil: List[Int])(_::_)

  /* Ex. 3.9 */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x+1)

  /* Ex. 3.10 */
  @tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }
  }


  /* Ex. 3.11 */
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_+_)
  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_*_)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((x, _) => x+1)

  /* Ex. 3.12 */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((x, xs) => xs::x)
  }

  /* Ex. 3.13 */
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))
  }
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  }

  /* Ex. 3.14 */
  def append[A](as: List[A], elem: A): List[A] = {
    foldRight2(as, List(elem))(_::_)
  }

  /* Ex. 3.15 */
  def concat[A](xsx: List[List[A]]): List[A] = {
    foldRight2(xsx, Nil: List[A])(_++_)
  }

  /* Ex. 3.16 */
  def addOne(lst: List[Int]): List[Int] = {
    lst match {
      case x::xs => (x+1)::addOne(xs)
      case Nil => Nil
    }
    // lst.map(_+1)
  }

  /* Ex. 3.17 */
  def doubleToString(lst: List[Double]): List[String] = {
    lst match {
      case x::xs => x.toString::doubleToString(xs)
      case Nil => Nil
    }
  }

  /* Ex. 3.18 */
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    // Can generalise last two patterns into a map function
    as match {
      case x::xs => f(x)::map(xs)(f)
      case Nil => Nil
    }
  }

  /* Ex. 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case x::xs => if (f(x)) x::filter(xs)(f) else filter(xs)(f)
      case Nil => Nil
    }
  }

  /* Ex. 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case x :: xs => f(x) ++ flatMap(xs)(f)
      case Nil => Nil
    }
  }

  /* Ex. 3.21 */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x=>if (f(x)) List(x) else Nil)
  }

  /* Ex. 3.22 */
  def addList(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (x::xs, y::ys) => x+y::addList(xs, ys)
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }

  /* Ex. 3.23 */
  def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B): List[B] = {
    (as, bs) match {
      case (x::xs, y::ys) => f(x, y)::zipWith(xs, ys)(f)
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }

  /* Ex. 3.24 */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup.sliding(sub.size).contains(sub)
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  /* Ex. 3.25 */
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right)=> 1 + size(left) + size(right)
    }
  }

  /* Ex. 3.26 */
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(a) => a
      case Branch(left, right)=> maximum(left) max maximum(right)
    }
  }

  /* Ex. 3.27 */
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }


  /* Ex. 3.28 */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(combine: (B,B) => B): B = {
    tree match {
      case Leaf(a) => f(a)
      case Branch(left, right) => combine(fold(left)(f)(combine), fold(right)(f)(combine))
    }
  }
  def size2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(1+_+_)
  }

  def maximum2(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }

  def depth2[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((a, b) => 1 + (a max b))
  }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a=> Leaf(f(a)): Tree[B])(Branch(_, _))
  }

}
