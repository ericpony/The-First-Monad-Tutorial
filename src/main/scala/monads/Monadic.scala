package monads

object EvalRevisited0 {
  type M[A] = A
  def pure[A](a: A): M[A] = a
  def bind[A, B](a: M[A], k: A => M[B]): M[B] = k(a)

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) =>
        pure(a)
      case Div(t, u) =>
        bind(eval(t), (a: Int) =>
          bind(eval(u), (b: Int) =>
            pure(a / b)))
    }

  def main(a: Array[String]): Unit = {
    println(eval(answer))
    println(eval(error))
  }
}

object EvalRevisited1 {
  type Exception = String
  trait M[A]
  case class Raise[A](e: Exception) extends M[A]
  case class Return[A](a: A) extends M[A]

  def pure[A](a: A): M[A] = Return(a)
  def bind[A, B](m: M[A], k: A => M[B]): M[B] =
    m match {
      case Raise(e)  => Raise(e)
      case Return(a) => k(a)
    }
  def raise[A](e: String): M[A] = Raise(e)

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) =>
        pure(a)
      case Div(t, u) =>
        bind(eval(t), (a: Int) =>
          bind(eval(u), (b: Int) =>
            if (b == 0)
              raise("divide by zero")
            else
              pure(a / b)))
    }

  def main(a: Array[String]): Unit = {
    println(eval(answer))
    println(eval(error))
  }
}

object EvalRevisited2 {
  type State = Int
  type M[A] = State => (A, State)

  def pure[A](a: A): M[A] = x => (a, x)
  def bind[A, B](m: M[A], k: A => M[B]): M[B] =
    x => {
      val (a, y) = m(x)
      val (b, z) = k(a)(y)
      (b, z)
    }
  def tick: M[Unit] = (x: Int) => ((), x + 1)

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) =>
        pure(a)
      case Div(t, u) =>
        bind(eval(t), (a: Int) =>
          bind(eval(u), (b: Int) =>
            bind(tick, (_: Unit) =>
              pure(a / b))))
    }

  def main(a: Array[String]): Unit = {
    println(eval(answer)(0))
  }
}

object EvalRevisited3 {
  type Output = String
  type M[A] = (Output, A)

  def pure[A](a: A): M[A] = ("", a)
  def bind[A, B](m: M[A], k: A => M[B]): M[B] = {
    val (x, a) = m
    val (y, b) = k(a)
    (x + y, b)
  }
  def output[A](s: String): M[Unit] = (s, ())

  def line(r: Term, a: Int): Output =
    r + "=" + a + "\n"

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) =>
        bind(output(line(s, a)), (_: Unit) =>
          pure(a))
      case Div(t, u) =>
        bind(eval(t), (a: Int) =>
          bind(eval(u), (b: Int) =>
            bind(output(line(s, a / b)), (_: Unit) =>
              pure(a / b))))
    }

  def main(a: Array[String]): Unit =
    println(eval(answer))
}