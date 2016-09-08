package hasheq

import org.scalacheck.Properties
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait TestSuite extends FunSuite with Checkers {

  protected def checkAll(props: Properties): Unit =
    for ((name, prop) <- props.properties) {
      test(name) { check(prop) }
    }

}