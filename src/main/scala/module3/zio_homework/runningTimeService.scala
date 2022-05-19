package module3.zio_homework

import module3.zioConcurrency.currentTime
import zio.{Has, ULayer, ZIO, ZLayer}
import zio.clock.Clock
import zio.console.{Console, putStrLn}

object runningTimeService {
	type RunningTimeService = Has[RunningTimeService.Service]

	object RunningTimeService {

		trait Service {
			def myPrintEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
		}

		class ServiceImpl extends Service {
			override def myPrintEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = {
				for {
					start  <- currentTime
					z 	   <- zio
					finish <- currentTime
					_ 	   <- putStrLn(s"Running time: ${finish - start}").orDie
				} yield z
			}
		}

		val live: ULayer[Has[Service]] = ZLayer.succeed(new ServiceImpl)

		// accessors
		def myPrintEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with RunningTimeService with Clock with Console, E, A] = {
			ZIO.accessM(_.get myPrintEffectRunningTime zio)
		}
	}

}