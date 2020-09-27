package make.zio

import zio._

object App extends zio.App {

  override def run(args: List[String]): zio.URIO[zio.ZEnv,ExitCode] = {
    val x = ZLayer.succeed(42)
    val y = ZLayer.succeed(42)
    val z = x ++ y
    // z.launch.as(ExitCode.success)
    // URIO(ExitCode.success)
    run.provideLayer(z).as(ExitCode.success)

    ZManaged.
  }

  def run: ZIO[Has[Int] with Has[Int], Nothing, Unit] =
    ZIO.accessM[Has[Int]](e => UIO(println(e)))


}