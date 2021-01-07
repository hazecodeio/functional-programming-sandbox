package ch04

/**
 * Explanation:
 *    - https://scalac.io/typeclasses-in-scala/
 *    - https://www.youtube.com/watch?v=bupBZKJT0EA&ab_channel=RocktheJVM
 *
 * Observations:
 *    - ad-hoc polymorphism
 *    - Implicit Monads make use of type classes
 */
object TypeClassesByMe {


  implicit object IntSummable extends Summable[Int] {
    override def sumElements(list: List[Int]): Int = list.sum
  }

  implicit object StringSummable extends Summable[String] {
    override def sumElements(list: List[String]): String = list.mkString
  }

  // implicit via def
  implicit def BoolSummable:Summable[Boolean] = (list:List[Boolean]) => list.reduce(_ || _)



  def main(args: Array[String]) = {
    val intSum1 = processMyListViaImplicitParam(List(1, 2, 3))
    val intSum2 = processMyListViaContextBounds(List(1, 2, 3))
    println(intSum1)
    println(intSum2)

    val strSum1 = processMyListViaImplicitParam(List("1", "2", "3"))
    val strSum2 = processMyListViaContextBounds(List("1", "2", "3"))
    println(strSum1)
    println(strSum2)

    println(processMyListViaContextBounds(List(true, true, false)))

  }

  /////////////////////

  def processMyListViaImplicitParam[T](list: List[T])(implicit summable: Summable[T]): T = summable.sumElements(list)

  def processMyListViaContextBounds[T: Summable](list: List[T]): T = implicitly[Summable[T]].sumElements(list)

  //Type Class; which will be used to define implicits
  trait Summable[T] {
    def sumElements(list: List[T]): T
  }

}
