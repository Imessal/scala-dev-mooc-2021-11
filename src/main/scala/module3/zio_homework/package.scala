package module3

import module3.zio_homework.runningTimeService.RunningTimeService
import pureconfig.{ConfigObjectSource, ConfigSource}
import zio.clock.Clock
import zio.console._
import zio.duration.durationInt
import zio.random._
import zio.{ExitCode, IO, Ref, Task, UIO, URIO, ZIO}

import java.io.IOException
import java.nio.file.Paths
import scala.language.postfixOps
import scala.util.Try

package object zio_homework {
	/**
	 * 1.
	 * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
	 * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
	 */

	private def zParseInt(number: String): IO[Option[Nothing], Int] = {
		ZIO.fromOption {
			Try(number.toInt).toOption
		}
	}

	lazy val guess = getStrLn.flatMap(zParseInt).tapError(_ => putStrLn("It's not a number :("))
	lazy val guessProgram: URIO[Console with Random, ExitCode] = {
		for {
			_ <- zio.console.putStrLn("Guess number?")
			number <- nextIntBounded(3)
			userNumber <- guess.eventually
			_ <- if (number == userNumber) {
				putStrLn("Right!")
			} else {
				putStrLn("Wrong :( Another try?") *> guessProgram
			}
		} yield ()
	}.exitCode

	/**
	 * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
	 *
	 */

	//???
	def doWhile[A](effect: Task[A]) = {
		effect.eventually
	}

	/**
	 * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
	 * и выведет его в консоль
	 * Используйте эффект "load" из пакета config
	 */

	def loadConfigOrDefault(configPath: String): Task[ConfigObjectSource] = {
		val path = Paths.get(configPath)
		Task {
			ConfigSource.file(path)
		}.tapError(_ => {
			for {
				default <- config.load
			} yield default
		})
	}


	/**
	 * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
	 * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
	 * на изменение этих сигнатур
	 */


	/**
	 * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
	 * Используйте сервис zio Random
	 */
	lazy val eff: ZIO[Random with Clock with Random, Nothing, Int] = {
		ZIO.sleep(1 second) *> zio.random.nextIntBounded(10)
	}

	/**
	 * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
	 */
	lazy val effects: Seq[ZIO[Random with Clock with Random, Nothing, Int]] = List.fill(10)(eff)


	/**
	 * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
	 * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
	 * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
	 */

	type appType = ZIO[Random with Clock with Random with Console, IOException, Unit]
	type innerAppType = ZIO[Random with Clock, Nothing, Int]

	lazy val zEffectsSum: innerAppType = effects.reduce[innerAppType] { case (z1, z2) =>
		for {
			v1 <- z1
			v2 <- z2
		} yield v1 + v2
	}

	lazy val app: appType = {
		for {
			_ <- putStrLn("Starting...")
			result <- zEffectsSum // закомментил для последнего задания result <- printEffectRunningTime(zEffectsSum)
			_ <- putStrLn(s"Result: $result")
		} yield ()
	}


	/**
	 * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
	 */

	val counter: UIO[Ref[Int]] = Ref.make(0)
	lazy val zEffectsSumSpeedUp: ZIO[Random with Clock, Nothing, Ref[Int]] = for {
		ref <- counter
		_ <- ZIO.foreachPar(effects) { eff =>
			eff.flatMap(value => ref.getAndUpdate(_ + value))
		}
	} yield {
		ref
	}

	lazy val appSpeedUp: ZIO[Console with Clock with Random, IOException, Unit] = {
		for {
			_ <- putStrLn("Starting...")
			resultRef <- zEffectsSumSpeedUp // закомментил для последнего задания result <- printEffectRunningTime(zEffectsSumSpeedUp)
			result <- resultRef.get
			_ <- putStrLn(s"Result: $result")
		} yield ()
	}


	/**
	 * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
	 * молжно было использовать аналогично zio.console.putStrLn например
	 */


	/**
	 * 6.
	 * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
	 *
	 *
	 */

	type AppEnv = Random with Clock with Random with Console with RunningTimeService
	val appWithTimeLogg: ZIO[AppEnv, IOException, Unit] = for {
		_ <- putStrLn("CONSEQUENT: \n")
		_ <- RunningTimeService.myPrintEffectRunningTime(app)
		_ <- putStrLn("\nPARALLEL:")
		_ <- RunningTimeService.myPrintEffectRunningTime(appSpeedUp)
	} yield ()

	/**
	 *
	 * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
	 */

	lazy val runApp = {
		appWithTimeLogg.provideSomeLayer[Clock with Random with Console](RunningTimeService.live).exitCode
	}

}
