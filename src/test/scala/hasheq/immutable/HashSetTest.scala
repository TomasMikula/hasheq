package hasheq
package immutable

import hasheq.std.int._

class HashSetTest extends TestSuite {

  checkAll(SetRepr.properties[HashSet, Int]("SetRepr[HashSet, Int]"))

  // test set of integers modulo 10
  implicit object IntEqMod10 extends Equiv[Int] {
    def mod10(i: Int): Int = {
      val r = i % 10
      if (r < 0) r + 10
      else r
    }
    def equiv(a: Int, b: Int): Boolean = mod10(a) == mod10(b)
  }
  implicit object IntHashMod10 extends Hash[Int, IntEqMod10.type] {
    def hash(a: Int): Int = IntEqMod10.mod10(a)
  }
  checkAll(Setoid.properties[HashSetoid, Int, IntEqMod10.type]("Setoid[HashSetoid, Int, IntEqMod10]"))

  // compilation check
  val x = HashSetoid(HashSetoid(HashSet(1)))
}
