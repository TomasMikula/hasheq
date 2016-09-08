package hasheq
package immutable

import scala.collection.AbstractIterable
import scala.language.higherKinds

/** Witness that for all types `V`, `M[K, V]` represents an immutable map
  * with keys of type `K` and values of type `V`.
  */
trait MapRepr[M[_, _], K] {
  def empty[V]: M[K, V]
  def fromIterable[V](col: Iterable[(K, V)])(implicit E: Equal[K]): M[K, V] = fromIterator(col.iterator)
  def fromIterator[V](it: Iterator[(K, V)])(implicit E: Equal[K]): M[K, V] =
    it.foldLeft(empty[V])((m, kv) => put(m, kv._1, kv._2))

  def size[V](m: M[K, V]): Int
  def isEmpty[V](m: M[K, V]): Boolean = size(m) == 0
  def get[V](m: M[K, V], k: K)(implicit E: Equal[K]): Option[V]

  def iterator[V](m: M[K, V]): Iterator[(K, V)]
  def keysIterator[V](m: M[K, V]): Iterator[K] = iterator(m).map(_._1)
  def valuesIterator[V](m: M[K, V]): Iterator[V] = iterator(m).map(_._2)

  def entries[V](m: M[K, V]): Iterable[(K, V)] = new AbstractIterable[(K, V)] { def iterator = MapRepr.this.iterator(m) }
  def keys[V](m: M[K, V]): Iterable[K] = new AbstractIterable[K] { def iterator = keysIterator(m) }
  def values[V](m: M[K, V]): Iterable[V] = new AbstractIterable[V] { def iterator = valuesIterator(m) }

  def keySet[S[_]](m: M[K, _]): KeySetBuilder[S] = KeySetBuilder()

  def put[V](m: M[K, V], k: K, v: V)(implicit E: Equal[K]): M[K, V]
  def updated[V](m: M[K, V], k: K, v: V)(combine: (V, V) => V)(implicit E: Equal[K]): M[K, V] = {
    get(m, k) match {
      case Some(v0) => put(m, k, combine(v0, v))
      case None     => put(m, k, v)
    }
  }

  final case class KeySetBuilder[S[_]]() {
    def apply[S0[_, _]](m: M[K, _])(implicit ev: S0[K, Equality.type] =:= S[K], S: Setoid[S0, K, Equality.type], E: Equal[K]): S[K] = ev(S.fromIterator(keysIterator(m)))
  }
}
