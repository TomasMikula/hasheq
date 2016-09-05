package hasheq
package std

object int {
  implicit def hashEqInstance: HashEq[Int] = new HashEq[Int] {
    def equal(x: Int, y: Int): Boolean = x == y
    def hash(x: Int): Int = x
  }
}
