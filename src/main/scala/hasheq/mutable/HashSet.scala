package hasheq
package mutable

/** This class implements mutable sets using a hashtable.
 */
class HashSet[A] private[hasheq] (contents: FlatHashTable.Contents[A])
extends FlatHashTable[A]
{
  initWithContents(contents)

  def this() = this(null)

  def size: Int = tableSize

  def contains(elem: A)(implicit A: Hash[A], E: Equal[A]): Boolean = containsElem(elem)

  def += (elem: A)(implicit A: Hash[A], E: Equal[A]): this.type = { addElem(elem); this }

  def -= (elem: A)(implicit A: Hash[A], E: Equal[A]): this.type = { removeElem(elem); this }

  def add(elem: A)(implicit A: Hash[A], E: Equal[A]): Boolean = addElem(elem)

  def remove(elem: A)(implicit A: Hash[A], E: Equal[A]): Boolean = removeElem(elem)

  def clear() = { clearTable() }

  override def iterator: Iterator[A] = super[FlatHashTable].iterator

  def foreach[U](f: A => U) = {
    var i = 0
    val len = table.length
    while (i < len) {
      val curEntry = table(i)
      if (curEntry ne null) f(entryToElem(curEntry))
      i += 1
    }
  }

  /** Toggles whether a size map is used to track hash map statistics.
   */
  def useSizeMap(t: Boolean) = if (t) {
    if (!isSizeMapDefined) sizeMapInitAndRebuild()
  } else sizeMapDisable()

}

object HashSet {
  def empty[A]: HashSet[A] = new HashSet[A]

  implicit def setReprInstance[A: Hash: Equal]: SetRepr[HashSet, A] = new SetRepr[HashSet, A] {
    def empty: HashSet[A] = HashSet.empty[A]

    def size(s: HashSet[A]): Int = s.size
    def find(s: HashSet[A], a: A): Option[A] = s.findElem(a)
    def iterator(s: HashSet[A]): Iterator[A] = s.iterator

    def add(s: HashSet[A], a: A): Boolean = s.add(a)
    def remove(s: HashSet[A], a: A): Boolean = s.remove(a)
  }
}

