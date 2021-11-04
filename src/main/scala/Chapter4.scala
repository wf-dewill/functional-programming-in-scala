object Chapter4 extends App {
  /*
  * Exception handling in programs
  * Scala has better wayss of handling exceptions than just a try catch block
  * Such as Option, Either, Try etc.
  * Pattern matchable
  * */

  /* Ex. 4.1 */
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }
    def orElse[B >: A](op: => Option[B]): Option[B] = map(Some(_)).getOrElse(op)
    def filter(f: A => Boolean): Option[A] = if (map(f).getOrElse(false)) this else None
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  /* Ex 4.2 */
  def variance(xs: Seq[Double]): Option[Double] = {
   Some(xs.sum/xs.size).flatMap(m => Some(xs.map(a =>math.pow(a - m, 2)).sum/xs.size))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /* Ex. 4.3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(elem => b.map(f(elem, _)))

  /* Ex. 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case x::xs => x flatMap {elem => sequence(xs) map (elem :: _)}
    }

  /* Ex. 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x::xs => f(x) flatMap {elem => traverse(xs)(f) map (elem :: _)}
    }
  }
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aa => aa)

  /* Ex. 4.6 */
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(b) => Left(b)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => Right(a)
        case Left(_) => b
      }
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      this flatMap {elem => b map {c => f(elem, c)}}
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }

  /* Ex. 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case x::xs => x flatMap {elem => sequence(xs) map (elem :: _) }
     }
  }

  def traverse[E, A, B](es: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] ={
    es match {
      case Nil => Right(Nil)
      case x::xs =>  x flatMap f flatMap {elem => traverse(xs)(f) map {elem :: _} }
    }
  }

  /* Ex. 4.8 */
  // Could change signature of map2 to
  // map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[(EE, E), C]
  // Or could change map2 to actually concatenate the strings, but thats less general
  //
}
