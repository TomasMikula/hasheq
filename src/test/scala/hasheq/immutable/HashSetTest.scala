package hasheq
package immutable

import hasheq.std.int._

class HashSetTest extends TestSuite {

  // test set of Int
  checkAll(SetRepr.properties[HashSet, Int]("SetRepr[HashSet, Int]"))

  // test set of Int modulo 10
  object Mod10
  implicit object IntEqMod10 extends Equiv[Int, Mod10.type] {
    def mod10(i: Int): Int = {
      val r = i % 10
      if (r < 0) r + 10
      else r
    }
    def equiv(a: Int, b: Int): Boolean = mod10(a) == mod10(b)
  }
  implicit object IntHashMod10 extends HashEq[Int, Mod10.type] {
    def hash(a: Int): Int = IntEqMod10.mod10(a)
  }
  checkAll(Setoid.genProperties[HashSetoid, Int, Mod10.type]("Setoid[HashSetoid, Int, Mod10]"))

  // check that the equivalence on HashSet is lawful
  checkAll(Equiv.properties[HashSet[Int]]())

  // check that the hash function on HashSet is lawful
  checkAll(HashEq.properties[HashSet[Int]]())

  // compilation check
  val x = HashSetoid(HashSetoid(HashSet(1)))
}
