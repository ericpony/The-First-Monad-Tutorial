package monads

object Lists {
  type M[A] = List[A]

  def pure[A](a: A): M[A] = List(a)
  def bind[A, B](m: M[A], k: A => M[B]): M[B] =
    m match {
      case Nil    => Nil
      case h :: t => k(h) ++ bind(t, k)
    }
  def zero[A]: M[A] = Nil
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

  // using List's for comprehension syntax
  def productFor[A, B](m: M[A], n: M[B]): M[(A, B)] =
    for {
      a <- m
      b <- n
    } yield (a, b)

  def main(a: Array[String]) = {
    println(product(List(1, 2, 3), List('a', 'b', 'c')))
    println(productFor(List(1, 2, 3), List('a', 'b', 'c')))
  }
}
