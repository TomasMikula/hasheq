package hasheq
package immutable

import scala.annotation.tailrec

/**
  * Note that each element insertion takes O(n) time, which means that creating a list map with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  */
private[hasheq] object ListMap {

  def empty[A, B]: ListMap[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  private object EmptyListMap extends ListMap[Any, Nothing]
}

/**
  * This class implements immutable maps using a list-based data structure. List map iterators and
  * traversal methods visit key-value pairs in the order whey were first inserted.
  *
  * Entries are stored internally in reversed insertion order, which means the newest key is at the
  * head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and `init`
  * are O(1). Other operations, such as inserting or removing entries, are also O(n), which makes
  * this collection suitable only for a small number of elements.
  *
  * Instances of `ListMap` represent empty maps; they can be either created by calling the
  * constructor directly, or by applying the function `ListMap.empty`.
  *
  * @tparam A the type of the keys contained in this list map
  * @tparam B the type of the values associated with the keys
  */
private[hasheq] sealed class ListMap[A, +B] extends Iterable[(A, B)] {

  override def size: Int = 0
  override def isEmpty: Boolean = true

  override def head: (A, B) = throw new NoSuchElementException("head of empty map")

  def get(key: A)(implicit A: Equiv[A]): Option[B] = None

  def contains(key: A)(implicit A: Equiv[A]): Boolean = get(key).isDefined

  def apply(key: A)(implicit A: Equiv[A]): B = get(key).get

  def updated[B1 >: B](key: A, value: B1)(implicit A: Equiv[A]): ListMap[A, B1] = new Node[B1](key, value)

  def +[B1 >: B](kv: (A, B1))(implicit A: Equiv[A]): ListMap[A, B1] = new Node[B1](kv._1, kv._2)
  def -(key: A)(implicit A: Equiv[A]): ListMap[A, B] = this

  def ++[B1 >: B](xs: Iterable[(A, B1)])(implicit A: Equiv[A]): ListMap[A, B1] =
    if (xs.isEmpty) this
    else xs.foldLeft(this: ListMap[A, B1])(_ + _)

  def iterator: Iterator[(A, B)] = {
    def reverseList = {
      var curr: ListMap[A, B] = this
      var res: List[(A, B)] = Nil
      while (!curr.isEmpty) {
        res = (curr.key, curr.value) :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  override def filter(p: ((A, B)) => Boolean): ListMap[A, B] = {
    @tailrec def go(cur: ListMap[A, B], acc: ListMap[A, B]): ListMap[A, B] =
      if(cur.isEmpty) acc
      else if(p((cur.key, cur.value))) go(cur.next, new acc.Node(cur.key, cur.value))
      else go(cur.next, acc)
    go(this, ListMap.empty)
  }

  override def filterNot(p: ((A, B)) => Boolean): ListMap[A, B] = {
    filter(p andThen (!_))
  }

  override def splitAt(n: Int): (ListMap[A, B], ListMap[A, B]) = {
    @tailrec def take(cur: ListMap[A, B], n: Int, acc: ListMap[A, B]): (ListMap[A, B], ListMap[A, B]) =
      if(n > 0 && !cur.isEmpty) take(cur.next, n-1, new acc.Node(cur.key, cur.value))
      else (acc, cur)

    take(this, n, ListMap.empty)
  }

  protected def key: A = throw new NoSuchElementException("key of empty map")
  protected def value: B = throw new NoSuchElementException("value of empty map")
  protected def next: ListMap[A, B] = throw new NoSuchElementException("next of empty map")

  override def stringPrefix = "ListMap"

  /**
    * Represents an entry in the `ListMap`.
    */
  protected class Node[B1 >: B](override protected val key: A,
                                override protected val value: B1) extends ListMap[A, B1] with Serializable {

    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[A, B1], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def apply(k: A)(implicit A: Equiv[A]): B1 = applyInternal(this, k)

    override def head: (A, B1) = (key, value)

    @tailrec private[this] def applyInternal(cur: ListMap[A, B1], k: A)(implicit A: Equiv[A]): B1 =
      if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else if (A.equiv(k, cur.key)) cur.value
      else applyInternal(cur.next, k)

    override def get(k: A)(implicit A: Equiv[A]): Option[B1] = getInternal(this, k)

    @tailrec private[this] def getInternal(cur: ListMap[A, B1], k: A)(implicit A: Equiv[A]): Option[B1] =
      if (cur.isEmpty) None
      else if (A.equiv(k, cur.key)) Some(cur.value)
      else getInternal(cur.next, k)

    override def contains(k: A)(implicit A: Equiv[A]): Boolean = containsInternal(this, k)

    @tailrec private[this] def containsInternal(cur: ListMap[A, B1], k: A)(implicit A: Equiv[A]): Boolean =
      if(cur.isEmpty) false
      else if (A.equiv(k, cur.key)) true
      else containsInternal(cur.next, k)

    override def updated[B2 >: B1](k: A, v: B2)(implicit A: Equiv[A]): ListMap[A, B2] = {
      val m = this - k
      new m.Node[B2](k, v)
    }

    override def +[B2 >: B1](kv: (A, B2))(implicit A: Equiv[A]): ListMap[A, B2] = {
      val m = this - kv._1
      new m.Node[B2](kv._1, kv._2)
    }

    override def -(k: A)(implicit A: Equiv[A]): ListMap[A, B1] = removeInternal(k, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListMap[A, B1], acc: List[ListMap[A, B1]])(implicit A: Equiv[A]): ListMap[A, B1] =
      if (cur.isEmpty) acc.last
      else if (A.equiv(k, cur.key)) (cur.next /: acc) { case (t, h) => new t.Node(h.key, h.value) }
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListMap[A, B1] = ListMap.this
  }
}
