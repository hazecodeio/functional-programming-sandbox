package _06_Effects


object Effects extends App {

  type Bait
  type Line
  type Fish

  import java.util

  val map = new util.HashMap[String, Int] {
    put("zero", 0)
  }

  val list = new util.ArrayList[String] {
    add("zero")
  }

  println(map.get("zero"))
  println(list.get(0))

  println(map.get("one"))

  /*list.get(1) match { // this will throw exception
    case null => println("Not found")
    case notNull => println(notNull)
  }*/

  import scala.util._

  Try(list.get(1)) match { // Wrap the except ina Try Effect
    case Failure(e) => println(e)
    case Success(value) => println(value)
  }

}
