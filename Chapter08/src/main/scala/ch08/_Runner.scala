package ch08

object _Runner extends App {


  /**
   * Added by me
   */
  object ApplicativeRunner {

    import Applicative._
    import scala.util._

    // CaseClass to hold the outcomes
    case class C(a: Int, b: Int, c: Int)

    println("----------------------- OptionApplicative ------------------")
    private val optionC: Option[C] = optionApplicative.map3(
      Some(1),
      Some(2),
      Some(3))(C) // Notice how we passed a CaseClass that will hold the outcome
    optionC.foreach(println)
    println

    println("----------------------- TryApplicative ------------------")
    def safeDivide(a: Int, b: Int) = Try(a / b)

    private val triedC: Try[C] = tryApplicative.map3(
      safeDivide(10, 5),
      safeDivide(20, 5),
      safeDivide(30, 5)
    )(C)
    triedC.foreach(println)
    println
  }

  ApplicativeRunner
}
