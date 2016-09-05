package hasheq
package mutable

private[hasheq] final class DefaultEntry[A, B](val key: A, var value: B)
      extends HashEntry[A, DefaultEntry[A, B]] with Serializable
{
  override def toString = chainString

  def chainString = {
    "(kv: " + key + ", " + value + ")" + (if (next != null) " -> " + next.toString else "")
  }
}
