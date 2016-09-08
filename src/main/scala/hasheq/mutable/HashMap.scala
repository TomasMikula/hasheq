package hasheq
package mutable

import scala.language.higherKinds

/** This class implements mutable maps using a hashtable.
 *
 *  @tparam A    the type of the keys contained in this hash map.
 *  @tparam B    the type of the values assigned to keys in this hash map.
 */
class HashMap[A, B] private[hasheq] (contents: HashTable.Contents[A, DefaultEntry[A, B]])
extends HashTable[A, DefaultEntry[A, B]]
{
  initWithContents(contents)

  type Entry = DefaultEntry[A, B]

  def empty: HashMap[A, B] = HashMap.empty[A, B]
  def clear() = { clearTable() }
  def size: Int = tableSize

  def this() = this(null)

  // contains and apply overridden to avoid option allocations.
  def contains(key: A)(implicit A: Hash[A], E: Equal[A]): Boolean = findEntry(key) != null

  def apply(key: A)(implicit A: Hash[A], E: Equal[A]): B = {
    val result = findEntry(key)
    if (result eq null) throw new NoSuchElementException()
    else result.value
  }

  def get(key: A)(implicit A: Hash[A], E: Equal[A]): Option[B] = {
    val e = findEntry(key)
    if (e eq null) None
    else Some(e.value)
  }

  def put(key: A, value: B)(implicit A: Hash[A], E: Equal[A]): Option[B] = {
    val e = findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  def update(key: A, value: B)(implicit A: Hash[A], E: Equal[A]): Unit = put(key, value)

  def remove(key: A)(implicit A: Hash[A], E: Equal[A]): Option[B] = {
    val e = removeEntry(key)
    if (e ne null) Some(e.value)
    else None
  }

  def += (kv: (A, B))(implicit A: Hash[A], E: Equal[A]): this.type = {
    val e = findOrAddEntry(kv._1, kv._2)
    if (e ne null) e.value = kv._2
    this
  }

  def -=(key: A)(implicit A: Hash[A], E: Equal[A]): this.type = { removeEntry(key); this }

  def iterator = entriesIterator map (e => ((e.key, e.value)))

  def foreach[U](f: ((A, B)) => U): Unit = foreachEntry(e => f((e.key, e.value)))

  /* Override to avoid tuple allocation in foreach */
  def keySet[S[_]](implicit S: SetRepr[S, A]): S[A] = S.fromIterator(keysIterator)

  /* Override to avoid tuple allocation in foreach */
  def values: scala.collection.Iterable[B] = new scala.collection.AbstractIterable[B] {
    def iterator = valuesIterator
    override def size = HashMap.this.size
    override def foreach[U](f: B => U) = valuesIterator foreach f
  }

  def keys: scala.collection.Iterable[A] = new scala.collection.AbstractIterable[A] {
    def iterator = keysIterator
    override def size = HashMap.this.size
    override def foreach[U](f: A => U) = keysIterator foreach f
  }

  /* Override to avoid tuple allocation */
  def keysIterator: Iterator[A] = new scala.collection.AbstractIterator[A] {
    val iter    = entriesIterator
    def hasNext = iter.hasNext
    def next()  = iter.next().key
  }

  /* Override to avoid tuple allocation */
  def valuesIterator: Iterator[B] = new scala.collection.AbstractIterator[B] {
    val iter    = entriesIterator
    def hasNext = iter.hasNext
    def next()  = iter.next().value
  }

  /** Toggles whether a size map is used to track hash map statistics.
   */
  def useSizeMap(t: Boolean) = if (t) {
    if (!isSizeMapDefined) sizeMapInitAndRebuild()
  } else sizeMapDisable()

  protected def createNewEntry[B1](key: A, value: B1): Entry = {
    new Entry(key, value.asInstanceOf[B])
  }

}

object HashMap {
  def empty[A, B]: HashMap[A, B] = new HashMap[A, B]

  implicit def mapReprInstance[K: Hash]: MapRepr[HashMap, K] = new MapRepr[HashMap, K] {
    def empty[V]: HashMap[K, V] = HashMap.empty

    def size[V](m: HashMap[K, V]): Int = m.size
    def get[V](m: HashMap[K, V], k: K)(implicit E: Equal[K]): Option[V] = m.get(k)
    def iterator[V](m: HashMap[K, V]): Iterator[(K, V)] = m.iterator
    def keysIterator[V](m: HashMap[K, V]): Iterator[K] = m.keysIterator
    def valuesIterator[V](m: HashMap[K, V]): Iterator[V] = m.valuesIterator
    def keys[V](m: HashMap[K, V]): Iterable[K] = m.keys
    def values[V](m: HashMap[K, V]): Iterable[V] = m.values

    def put[V](m: HashMap[K, V], k: K, v: V)(implicit E: Equal[K]): Option[V] = m.put(k, v)
  }
}
