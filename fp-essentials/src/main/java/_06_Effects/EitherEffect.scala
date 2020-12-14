package _06_Effects

trait EitherEffectTryOut {

  type OldFormat
  type NewFormat
  type Money

  val right = Right(10)
  val left: Either[String, Int] = Left[String, Int]("I'm left")
  val i = 100
  val either: Either[String, Int] = Either.cond(i > 10, i, "i is greater then 10")
  val right1 = Right(10)

  takeOne(left)
  val left1: Either[String, BigDecimal] = Left("HoHoHo").withRight[BigDecimal]

  def runSimulation(): Either[OldFormat, NewFormat]

  def runPowerPlant(): Either[Long, Money]

  right1.withLeft[String]

  def takeOne(l: Either[String, Int]): Unit

  if (either.isRight) println("Got right")
  if (either.isLeft) println("Got left")

  either match {
    case Left(value) => println(s"Got Left value $value")
    case Right(value) => println(s"Got Right value $value")
  }


  if (either.contains("boo")) println("Is Right and contains 'boo'")
  if (either.exists(_ > 10)) println("Is Right and > 10")
  if (either.forall(_ > 10)) println("Is Left or > 10")


  either.getOrElse("Default value for the left side")

  def either(i: Int): Boolean = Either.cond(i > 10, i * 10, new IllegalArgumentException("Give me more")).forall(_ < 100)
}

trait FishingEitherExample {

  import Effects._

  trait plain {
    val buyBait: String => Bait
    val makeBait: String => Bait
    val castLine: Bait => Line
    val hookFish: Line => Fish

    def goFishing(bestBaitForFishOrCurse: Either[String, String]): Either[String, Fish] =
      bestBaitForFishOrCurse.map(buyBait).map(castLine).map(hookFish)
  }

  trait flat {
    val buyBait: String => Either[String, Bait]
    val makeBait: String => Either[String, Bait]
    val castLine: Bait => Either[String, Line]
    val hookFish: Line => Either[String, Fish]


    // By me
    def goFishingWithFlatMap(bestBaitForFishOrCurse: Either[String, String]): Either[String, Fish] = {
      bestBaitForFishOrCurse
        .flatMap(buyBait)
        .fold(s => makeBait(s), s => Right(s)) // ToDo: Check for correctness
        .flatMap(castLine)
        .flatMap(hookFish)
    }

    def goFishingWithForComprehension(bestBaitForFishOrCurse: Either[String, String]): Either[String, Fish] = for {
      baitName <- bestBaitForFishOrCurse
      bait <- buyBait(baitName).fold(_ => makeBait(baitName), Right(_))
      line <- castLine(bait)
      fish <- hookFish(line)
    } yield fish
  }

}

