package hasheq
package std

object int {
  implicit def equalInstance: Equal[Int] = new Equal[Int] {
    def equiv(x: Int, y: Int): Boolean = x == y
  }

  implicit def hashInstance: Hash[Int] = new Hash[Int] {
    def hash(x: Int): Int = x
  }
}
