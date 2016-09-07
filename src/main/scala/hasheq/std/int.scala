package hasheq
package std

object int {
  implicit def equalInstance: Equal[Int] = new Equal[Int] {
    def equal(x: Int, y: Int): Boolean = x == y
  }

  implicit def hashInstance: Hash[Int, Equal[Int]] = new Hash[Int, Equal[Int]] {
    def hash(x: Int): Int = x
  }
}
