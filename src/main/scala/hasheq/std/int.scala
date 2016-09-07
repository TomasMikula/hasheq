package hasheq
package std

object int {
  implicit def hashEqualInstance: HashEq[Int] with Equal[Int] = new HashEq[Int] with Equal[Int] {
    def equal(x: Int, y: Int): Boolean = x == y
    def hash(x: Int): Int = x
  }
}
