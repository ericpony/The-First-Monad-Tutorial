package monads

trait Term
case class Con(n: Int) extends Term
case class Div(t1: Term, t2: Term) extends Term

object Term {
  val answer: Term =
    Div(
      Div(
        Con(1932),
        Con(2)
      ),
      Con(23)
    )

  val error: Term =
    Div(
      Con(1),
      Con(0)
    )
}
import Term._

object Eval0 {
  def eval(t: Term): Int =
    t match {
      case Con(a)    => a
      case Div(t, u) => eval(t) / eval(u)
    }

  def main(a: Array[String]): Unit = {
    println(eval(answer))
    println(eval(error))
  }
}

object Eval1 {
  type Exception = String
  trait M[A]
  case class Raise[A](e: Exception) extends M[A]
  case class Return[A](a: A) extends M[A]

  def eval[A](t: Term): M[Int] =
    t match {
      case Con(a) => Return(a)
      case Div(t, u) => eval(t) match {
        case Raise(e) => Raise(e)
        case Return(a) =>
          eval(u) match {
            case Raise(e) => Raise(e)
            case Return(b) =>
              if (b == 0)
                Raise("divide by zero")
              else
                Return(a / b)
          }
      }
    }

  def main(a: Array[String]): Unit = {
    println(eval(answer))
    println(eval(error))
  }
}

object Eval2 {
  type State = Int
  type M[A] = State => (A, State)

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) => x => (a, x)
      case Div(t, u) => x =>
        val (a, y) = eval(t)(x)
        val (b, z) = eval(u)(y)
        (a / b, z + 1)
    }

  def main(a: Array[String]): Unit = {
    println(eval(answer)(0))
  }
}

object Eval3 {
  type Output = String
  type M[A] = (Output, A)

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) => (line(s, a), a)
      case Div(t, u) =>
        val (x, a) = eval(t)
        val (y, b) = eval(u)
        (x + y + line(s, a / b), a / b)
    }

  def line(t: Term, a: Int): Output =
    t + "=" + a + "\n"

  def main(a: Array[String]): Unit = {
    println(eval(answer))
  }
}

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def bind[A, B](a: M[A], f: A => M[B]): M[B]
}

object EvalRevisited {
}

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

object Async {
  import concurrent._
  // the default threadpool
  import ExecutionContext.Implicits.global
  import duration._

  type M[A] = () => A
  def pure[A](a: A): M[A] = () => a
  def bind[A, B](m: M[A], k: A => M[B]): M[B] =
    () => {
      val a = m()
      k(a)()
    }
  def spawn[A](m: M[A]): M[A] = {
    val a = Future { m() }
    () => Await.result(a, 5.seconds)
  }
  def divide(a: Int, b: Int): Int = {
    Thread.sleep(1000)
    a / b
  }

  def eval(s: Term): M[Int] =
    s match {
      case Con(a) =>
        pure(a)
      case Div(t, u) =>
        val x = spawn(eval(t))
        val y = spawn(eval(u))
        bind(x, (a: Int) =>
          bind(y, (b: Int) =>
            pure(divide(a, b))))
    }

  val balanced =
    Div(Div(Con(18), Con(3)), Div(Con(10), Con(5)))

  def par = {
    val a = Future { divide(18, 3) }
    val b = Future { divide(10, 5) }
    Await.result(a, 5.seconds) / Await.result(b, 5.seconds)
  }

  def main(a: Array[String]): Unit = {
    //println(Time(par))
    println(Time(eval(balanced)()))
  }
}

object Time {
  def apply[A](a: => A): (Long, A) = {
    val start = System.nanoTime
    val aa = a
    (System.nanoTime - start, aa)
  }
}

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

object Options {

}