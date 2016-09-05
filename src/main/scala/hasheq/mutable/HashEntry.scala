package hasheq
package mutable

private[hasheq] trait HashEntry [A, E] {
  val key: A
  var next: E = _
}
