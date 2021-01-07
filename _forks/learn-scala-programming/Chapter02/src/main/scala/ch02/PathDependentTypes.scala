package ch02

/**
 * PathDependentTypes work in combination with Inner Classes
 *
 * Observation:
 *    - Looks like in Scala, Objects of InnerClasses belong to one instance of its enclosing class
 *      -- Unlike Java
 *    - If you try to pass in an Object of an InnerClass to a method of a different enclosing Object of the enclosing class, it'll throw an error.
 *      -- Unlike Java
 *    - PathDependentTypes are for cross sharing
 */
object PathDependentTypes {

  final case class Lock() {

    final case class Key()

    def open(key: Key): Lock = this // Lock.this

    def close(key: Key): Lock = this // Lock.this

    def openWithMaster(key: Lock#Key): Lock = this // An example of PathDependentType; the usage of '#'

    def makeKey: Key = new Key

    def makeMasterKey: Lock#Key = new Key
  }

  val blue: Lock = Lock()
  val red: Lock = Lock()
  val blueKey: blue.Key = blue.makeKey
  val anotherBlueKey: blue.Key = blue.makeKey
  val redKey: red.Key = red.makeKey

  blue.open(blueKey)
  blue.open(anotherBlueKey)
  // blue.open(redKey) // compile error
  // red.open(blueKey) // compile error

  val masterKey: Lock#Key = red.makeMasterKey

  blue.openWithMaster(masterKey)
  red.openWithMaster(masterKey)

}
