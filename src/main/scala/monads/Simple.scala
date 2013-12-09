package monads

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
