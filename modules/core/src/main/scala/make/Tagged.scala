package make

object tagged {

  case class :@:[A, B](b: B)

  type X = Int :@: List[String]
}