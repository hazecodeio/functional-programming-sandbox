package ch08

object _Runner extends App {


  /**
   * Added by me
   */
  object ApplicativeRunner {

    import Applicative._

    import scala.util._

    /*
     * CaseClass to hold the outcomes
     * Reason:
     *    - case classes have apply() method
     *    - passing it as a function will result in matching f: (A,B,C) => D
     */
    case class C(a: Int, b: Int, c: Int)

    println("----------------------- OptionApplicative ------------------")
    private val optionC: Option[C] = optionApplicative.map3(
      Some(1),
      Some(2),
      Some(3))(C) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    optionC.foreach(println)
    println

    private val optionList: Option[List[Int]] = optionApplicative.map3(
      Some(1),
      Some(2),
      Some(3))((a,b,c) => a::b::c::Nil) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    optionList.foreach(println)
    println

    private val optionSum: Option[Int] = optionApplicative.map3(
      Some(1),
      Some(2),
      Some(3))((a,b,c) => a+b+c) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    optionSum.foreach(println)
    println

    println("----------------------- TryApplicative ------------------")
    def safeDivide(a: Int, b: Int) = Try(a / b)

    private val triedC: Try[C] = tryApplicative.map3(
      safeDivide(10, 5),
      safeDivide(20, 5),
      safeDivide(30, 5)
    )(C)
    triedC.foreach(println) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    println

    private val triedList: Try[List[Int]] = tryApplicative.map3(
      safeDivide(10, 5),
      safeDivide(20, 5),
      safeDivide(30, 5)
    )((a,b,c) => a::b::c::Nil)
    triedList.foreach(println) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    println

    private val triedSumOfDiv: Try[Int] = tryApplicative.map3(
      safeDivide(10, 5),
      safeDivide(20, 5),
      safeDivide(30, 5)
    )((a,b,c) => a+b+c)
    triedSumOfDiv.foreach(println) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    println
  }

  ApplicativeRunner

  object ApplicativeWithImplicitConverters{

    import _ImplicitConversions._;
    case class C(a: Int, b: Int, c: Int)

    println("----------------------- OptionApplicative ------------------")
    private val optionC: Option[C] =
      Option(1)
        .map3( // Notice how map3 is called on Option(1)
          Some(2),
          Some(3))(C) // Notice how we passed a CaseClass's CompanionObject that will hold the outcome. This is actually C.apply
    optionC.foreach(println)
    println
  }
  ApplicativeWithImplicitConverters
}
