import scala.annotation.tailrec

object Chapter2 extends App {

  /**
   * Tail recursion, polymorphic functions and functional programming*/

  /* Ex. 2.1 */
  def fib(a: Int): Int = {
    @tailrec def inner(c: Int, d: Int, i: Int): Int = if (i == a) c else inner(d, c + d, i + 1)
    inner(0, 1, 0)
  }

  /* Ex. 2.2 */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
//    as.zip(as.tail).forall(a => ordered(a._1, a._2))
    as.sliding(2).forall(a => ordered(a(0), a(1)))  // I prefer this
//    as.sliding(2).forall {case Array(a, b) => ordered(a, b)}
  }


  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  /* Ex. 2.3 */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  /* Ex. 2.4 */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /* Ex. 2.5 */
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))


}
