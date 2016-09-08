package hasheq.immutable

import hasheq.{Hash, TestSuite}
import hasheq.std.int._

class SetoidTest extends TestSuite {

  // check that the equivalence on sets defined in Setoid is lawful
  checkAll(hasheq.Equiv.properties[HashSet[Int]](HashSet.setoidInstance[Int].contentEquivalence))

  // check that the hash function on sets defined in Setoid is lawful
  checkAll(Hash.properties(HashSet.setoidInstance[Int].contentHash))

}
