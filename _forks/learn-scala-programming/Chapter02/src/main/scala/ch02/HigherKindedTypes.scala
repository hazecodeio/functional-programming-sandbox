package ch02

/**
 * Link: https://www.baeldung.com/scala/higher-kinded-types
 */
object HigherKindedTypes {

  import scala.language.higherKinds

  sealed trait Container[C] {
    def contents: C
  }

  case class Glass[C](contents: C) extends Container[C]

  case class Jar[C](contents: C) extends Container[C]

  def fillGlass[C](c: C): Glass[C] = Glass(c)

  def fillJar[C](c: C): Jar[C] = Jar(c)

  // Higher Kinded Type: A type that abstracts over another type
  sealed trait Filler[CC[_]] {
    def fill[C](c: C): CC[C]
  }

  object GlassFiller extends Filler[Glass] {
    override def fill[C](c: C): Glass[C] = Glass(c)
  }

  object JarFiller extends Filler[Jar] {
    override def fill[C](c: C): Jar[C] = Jar(c)
  }

  def fill[C, G[_]](c: C)(F: Filler[G]): G[C] = F.fill(c)

  val fullGlass: Glass[Int] = fill(100)(GlassFiller)
  val fullJar: Jar[Int] = fill(200)(JarFiller)
}
