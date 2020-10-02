package make.zio

import zio._

object implicits {

  // ZManaged[(A, B, C, D)] => ZLayer[Has[A] with Has[B]]
  // val x = Bind[A].asLayer()
  // implicit make (impl): Make[Zlayer, Hash[A] with Has[B]]
}