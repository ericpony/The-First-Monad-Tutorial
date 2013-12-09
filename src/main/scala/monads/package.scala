package object monads {
  trait Term
  case class Con(n: Int) extends Term
  case class Div(t1: Term, t2: Term) extends Term

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