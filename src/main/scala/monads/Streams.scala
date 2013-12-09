package monads

object Streams {
  type M[A] = Stream[A]

  def pure[A](a: A): M[A] = Stream(a)
  def bind[A, B](m: M[A], k: A => M[B]): M[B] =
    m match {
      case Stream() => Stream()
      case h #:: t  => k(h) ++ bind(t, k)
    }
  def zero[A]: M[A] = Stream()
  def plus[A](m: M[A], n: M[A]): M[A] = m ++ n

  def subsets[A](as: List[A]): M[List[A]] =
    as match {
      case Nil =>
        pure(Nil)
      case h :: t =>
        subsets(t) ++
          bind(subsets(t), (x: List[A]) => pure(h :: x))
    }

  def product[A, B](m: M[A], n: M[B]): M[(A, B)] =
    bind(m, (a: A) =>
      bind(n, (b: B) =>
        pure((a, b))))

  def productFor[A, B](m: M[A], n: M[B]): M[(A, B)] =
    for {
      a <- m
      b <- n
    } yield (a, b)

  def main(a: Array[String]) = {
    println(product(Stream(1, 2, 3), Stream('a', 'b', 'c')).toList)
    println(productFor(Stream(1, 2, 3), Stream('a', 'b', 'c')).toList)
  }
}
