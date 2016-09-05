package hasheq
package immutable

import org.scalacheck.Properties
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import hasheq.std.int._

class HashSetTest extends FunSuite with Checkers {

  private def checkAll(props: Properties): Unit =
    for ((name, prop) <- props.properties) {
      test(name) { check(prop) }
    }

  checkAll(SetRepr.properties[HashSet, Int]("SetRepr[HashSet, Int]"))
}
