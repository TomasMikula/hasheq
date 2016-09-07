package hasheq
package immutable

import scala.annotation.tailrec

/**
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2) time. This makes the builder suitable only for a small number of
  * elements.
  */
private[hasheq] object ListSet {

  private object EmptyListSet extends ListSet[Any]
  private[hasheq] def emptyInstance: ListSet[Any] = EmptyListSet

  def empty[A]: ListSet[A] = EmptyListSet.asInstanceOf[ListSet[A]]
}

/**
  * This class implements immutable sets using a list-based data structure. List set iterators and
  * traversal methods visit elements in the order whey were first inserted.
  *
  * Elements are stored internally in reversed insertion order, which means the newest element is at
  * the head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and
  * `init` are O(1). Other operations, such as inserting or removing entries, are also O(n), which
  * makes this collection suitable only for a small number of elements.
  *
  * Instances of `ListSet` represent empty sets; they can be either created by calling the
  * constructor directly, or by applying the function `ListSet.empty`.
  *
  * @tparam A the type of the elements contained in this list set
  */
private[hasheq] sealed class ListSet[A] extends Iterable[A] {

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def contains(elem: A)(implicit A: Equiv[A]): Boolean = false

  def +(elem: A)(implicit A: Equiv[A]): ListSet[A] = new Node(elem)
  def -(elem: A)(implicit A: Equiv[A]): ListSet[A] = this

  def ++(xs: scala.collection.GenTraversableOnce[A])(implicit A: Equiv[A]): ListSet[A] =
    if (xs.isEmpty) this
    else xs.foldLeft(this) (_ + _)

  def iterator: Iterator[A] = {
    def reverseList = {
      var curr: ListSet[A] = this
      var res: List[A] = Nil
      while (!curr.isEmpty) {
        res = curr.elem :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  /** Doesn't check for presence (i.e. assumes absence). */
  private def addInternal(a: A): ListSet[A] = new Node(a)

  private def filterImpl(p: A => Boolean, isFlipped: Boolean): ListSet[A] = {
    var res = ListSet.empty[A]
    for (x <- this)
      if (p(x) != isFlipped) res = res.addInternal(x)
    res
  }
  override def filter(p: A => Boolean): ListSet[A] = filterImpl(p, isFlipped = false)
  override def filterNot(p: A => Boolean): ListSet[A] = filterImpl(p, isFlipped = true)

  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  protected def next: ListSet[A] = throw new NoSuchElementException("next of empty set")
  override def head: A = elem
  override def tail: ListSet[A] = next

  override def stringPrefix = "ListSet"

  /**
    * Represents an entry in the `ListSet`.
    */
  protected class Node(override protected val elem: A) extends ListSet[A] with Serializable {

    override def size = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A)(implicit A: Equiv[A]) = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: ListSet[A], e: A)(implicit A: Equiv[A]): Boolean =
      !n.isEmpty && (A.equiv(n.elem, e) || containsInternal(n.next, e))

    override def +(e: A)(implicit A: Equiv[A]): ListSet[A] = if (contains(e)) this else new Node(e)

    override def -(e: A)(implicit A: Equiv[A]): ListSet[A] = removeInternal(e, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListSet[A], acc: List[ListSet[A]])(implicit A: Equiv[A]): ListSet[A] =
      if (cur.isEmpty) acc.last
      else if (A.equiv(k, cur.elem)) (cur.next /: acc) { case (t, h) => new t.Node(h.elem) }
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListSet[A] = ListSet.this

    override def last: A = elem
    override def init: ListSet[A] = next
  }
}
