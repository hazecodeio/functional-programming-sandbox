package _06_Effects

import java.io.IOError
import scala.io.StdIn.readLine
import scala.util._

trait TryEffect {
  val success = Success("Well")
  val failure = Failure(new Exception("Not so well"))

  val firstTry: Try[String] = try {
    Success(readLine())
  } catch {
    case err: IOError => Failure(err)
  }

  val secondTry = Try(readLine())

  val line = Try {
    val line = readLine()
    println(s"Got $line from console")
    line
  }

  if (line.isSuccess) println(s"The line was ${line.get}")
  if (line.isFailure) println(s"There was a failure")

  line.filter(_.nonEmpty)

  line.collect { case s: String => s * 10 }

  line.transform(
    (l: String) => Try(println(l)), // 1st param
    (ex: Throwable) => Try(throw ex) // 2nd param
  )

  val result = firstTry orElse secondTry orElse failure orElse success

  line.recover {
    case ex: NumberFormatException => Math.PI
  }

  line.recoverWith {
    case ex: NoSuchElementException => Try(retryAfterDelay)
  }

  def retryAfterDelay = ???

}

trait FishingTryExample {

  import Effects._

  trait plain { // Functions emitting regular types
    val buyBait: String => Bait
    val makeBait: String => Bait
    val castLine: Bait => Line
    val hookFish: Line => Fish

    def goFishing(bestBaitForFishOrCurse: Try[String]): Try[Fish] =
      bestBaitForFishOrCurse.map(buyBait).map(castLine).map(hookFish)
  }

  trait flat { // functions emitting "Effect" types
    val buyBait: String => Try[Bait]
    val makeBait: String => Try[Bait]
    val castLine: Bait => Try[Line]
    val hookFish: Line => Try[Fish]

    def goFishing(bestBaitForFishOrCurse: Try[String]): Try[Fish] = for {
      baitName <- bestBaitForFishOrCurse
      bait <- buyBait(baitName).fold(_ => makeBait(baitName), Success(_)) // ToDo - compare fold() to transform()
      line <- castLine(bait)
      fish <- hookFish(line)
    } yield fish
  }

}

