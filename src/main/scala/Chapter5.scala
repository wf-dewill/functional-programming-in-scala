import Chapter5.Stream.{cons, empty}

object Chapter5 extends App {
  // Lazy lists/ lazy structures fuse sequences of transforms for efficiency
  // Add lazy keyword before val/var to have scala delay eval until its called, then store result, rather than eval at runtime.

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    /* Ex. 5.1 */
    def toList: List[A] = {
      this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
      }
    }

    /* Ex. 5.2 */
    def take(n: Int): List[A] = {
      this match {
        case Empty => Nil
        case Cons(h, t) => if (n > 0) h() :: t().take(n-1) else Nil
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) => if (n == 0) Cons(h, t) else t().drop(n-1)
      }
    }

    /* Ex. 5.3 */
    def takeWhile(p: A => Boolean): Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
      }
    }

    def exists(p: A => Boolean): Boolean =
//      this match {
//        case Cons(h, t) => p(h()) || t().exists(p)
//        case _ => false
//      }
      foldRight(false)((a, b) => p(a) || b)


    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Empty => z
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
      }
    }

    /* Ex. 5.4 */
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    /* Ex. 5.5 */
    def takeWhile2(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)
    }

    /* Ex. 5.6 */
    def headOption2: Option[A] = {
      foldRight(None: Option[A])((a, _) => Some(a))
    }

    /* Ex. 5.7 */
    def map[B](f: A => B): Stream[B] = {
      foldRight(empty[B])((a, b) => cons(f(a), b))
    }

    def filter(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
    }

    def append[B >: A](element: => Stream[B]): Stream[B] = {
      foldRight(element)(cons(_, _))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(empty[B])((a, b) => f(a).append(b))
    }

    /* Ex. 5.13 */
    // no idea
    def mapUnfold[B](f: A => B): Stream[B] = {
      unfold(this){
        case Cons(a, b) => Some(f(a()), b())
        case _ => None
      }
    }

    def takeUnfold(n: Int): List[A] = {
      unfold((this, n)){
        case (Cons(a, b), num) => if (num == 0) None else Some(a(), (b(), num-1))
        case _ => None
      }.toList
    }

    def takeWhileUnfold(p: A => Boolean): List[A] = {
      unfold(this){
        case Cons(a, b) => if (p(a())) Some(a(), b()) else None
        case _ => None
      }.toList
    }

    def zipWith[B, C](st: => Stream[B])(f: (A, B) => C): Stream[C] = {
      unfold((this, st)){
        case (Cons(a, b), Cons(c, d)) => Some((f(a(), c()), (b(), d())))
        case _ => None
      }
    }

    def zipAll[B, C](st: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, st)){
        case (Cons(a, b), Cons(c, d)) => Some((Some(a()), Some(c())), (b(), d()))
        case (Cons(a, b), Empty) => Some((Some(a()), None), (b(), Empty))
        case (Empty, Cons(c, d)) => Some((None, Some(c())), (Empty, d()))
        case _ => None
      }
    }

    /* Ex. 5.14 */
    def startsWith[B >: A](s: Stream[B]): Boolean = {
      this.zipWith(s){case (a, b) => (a, b)}.forAll{case (a, b) => a == b}
    }

    /* Ex. 5.15 */
    def tails: Stream[Stream[A]] = {
      unfold(this){
        case Cons(a, b) =>  Some(cons(a(), b()), b())
        case _ => None
      }.append(Stream(Empty))
    }

    def hasSubsequence[B >: A](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

    /* Ex. 5.16 */
    def scanRight[B >: A](z: => B)(f: (A, => B) => B): Stream[B] = {
      this.foldRight((z, Stream(z)))(
        (a, b) =>
          (f(a, b._1) ,cons(f(a, b._1), b._2))
      )._2

    }

  }





  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] // () => A, body is a thunk. Just use :=> syntax

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl // so we don't eval each time
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*)) // unpacking
    }

  }
  def ones: Stream[Int] = cons(1, ones)

  /* Ex. 5.8 */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /* Ex. 5.9 */
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  /* Ex. 5.10 */
  def fibs: Stream[Int] = {
    def inner(a: Int, b: Int): Stream[Int] = {
      cons(a, inner(b, a+b))
    }
    inner(0, 1)
  }

  /* Ex. 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map{case (a: A, s: S) => cons(a, unfold(s)(f))}.getOrElse(empty)

    /*
    *  f(z) match {
    *   case Some((a, s)) => cons(a, unfold(s)(f))
    *   case None => empty
    * }
    * More readable, no getOrElse
    * */

  }

  /* Ex. 5.12 */
  def fibs2: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1+s._2))))
  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))
  def constant2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s)))
  def ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))





  val myStream: Stream[Int] = Stream(6,5)

 assert(myStream.toList == List(6,5))
  assert(myStream.take(1) == List(6))
  assert(myStream.drop(2).toList == List())
  assert(myStream.takeWhile(_%2 == 0).toList == List(6))
  assert(myStream.exists(_%2 == 0))
  assert(myStream.forAll(30%_== 0))
  assert(myStream.takeWhile2(_%2 == 0).toList == List(6))
  assert(myStream.filter(_%2 == 0).toList == List(6) )
  assert(myStream.append(Stream(683)).toList == List(6,5, 683))
  assert(fibs.take(9) == List(0, 1, 1, 2, 3, 5, 8 , 13, 21))
  assert(fibs2.take(9) == List(0, 1, 1, 2, 3, 5, 8 , 13, 21))
  assert(from2(1).take(5) == List(1,2,3,4,5))
  assert(constant2(1).take(3) == List(1,1,1))
  assert(ones2.take(3) == List(1,1,1))
  assert(Stream(1,2,3).takeUnfold(2) == List(1,2))
  assert(myStream.takeUnfold(2) == List(6,5))
  assert(Stream(5,4,6,3).startsWith(Stream(5, 4)))
  assert(Stream(1,2,3).tails.map(_.toList).toList == List(List(1,2,3), List(2,3), List(3), List()))

}
