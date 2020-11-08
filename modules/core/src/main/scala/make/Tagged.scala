package make

object tagged {

  case class :@:[A, B](a: A)

  implicit class Syntax[A](val a: A) extends AnyVal {
    def tagged[B]: A :@: B = :@:(a)
  }
}