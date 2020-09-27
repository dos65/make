package make.cats.effect

package object effect {

  type IOResource[A] = Resource[IO, A]

  
}
