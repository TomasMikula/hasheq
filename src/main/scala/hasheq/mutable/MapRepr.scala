package hasheq
package mutable

import scala.language.higherKinds

/** Witness that for all types `V`, `M[K, V]` represents a mutable map
  * with keys of type `K` and values of type `V`.
  */
trait MapRepr[M[_, _], K] {
  def empty[V]: M[K, V]
  def fromIterable[V](col: Iterable[(K, V)])(implicit E: Equal[K]): M[K, V] = fromIterator(col.iterator)
  def fromIterator[V](it: Iterator[(K, V)])(implicit E: Equal[K]): M[K, V] = {
    val m = empty[V]
    it.foreach(kv => put(m, kv._1, kv._2))
    m
  }

  def size[V](m: M[K, V]): Int
  def isEmpty[V](m: M[K, V]): Boolean = size(m) == 0
  def get[V](m: M[K, V], k: K)(implicit E: Equal[K]): Option[V]
  def iterator[V](m: M[K, V]): Iterator[(K, V)]
  def keysIterator[V](m: M[K, V]): Iterator[K]
  def valuesIterator[V](m: M[K, V]): Iterator[V]
  def keySet[S[_]](m: M[K, _])(implicit ev: SetRepr[S, K]): S[K] = ev.fromIterator(keysIterator(m))
  def keys[V](m: M[K, V]): Iterable[K]
  def values[V](m: M[K, V]): Iterable[V]

  def put[V](m: M[K, V], k: K, v: V)(implicit E: Equal[K]): Option[V]
  def update[V](m: M[K, V], k: K, v: V)(combine: (V, V) => V)(implicit E: Equal[K]): Unit = {
    get(m, k) match {
      case Some(v0) => put(m, k, combine(v0, v))
      case None     => put(m, k, v)
    }
  }
}
