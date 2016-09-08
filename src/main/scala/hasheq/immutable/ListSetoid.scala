package hasheq
package immutable

import scala.annotation.tailrec

/**
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2) time. This makes the builder suitable only for a small number of
  * elements.
  */
private[hasheq] object ListSetoid {

  private object EmptyListSetoid extends ListSetoid[Any, Any]
  private[hasheq] def emptyInstance: ListSetoid[Any, Any] = EmptyListSetoid

  def empty[A, Eq]: ListSetoid[A, Eq] = EmptyListSetoid.asInstanceOf[ListSetoid[A, Eq]]
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
private[hasheq] sealed class ListSetoid[A, Eq] extends Iterable[A] {

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def contains(elem: A)(implicit A: Equiv[A, Eq]): Boolean = false

  def +(elem: A)(implicit A: Equiv[A, Eq]): ListSetoid[A, Eq] = new Node(elem)
  def -(elem: A)(implicit A: Equiv[A, Eq]): ListSetoid[A, Eq] = this

  def ++(xs: scala.collection.GenTraversableOnce[A])(implicit A: Equiv[A, Eq]): ListSetoid[A, Eq] =
    if (xs.isEmpty) this
    else xs.foldLeft(this) (_ + _)

  def iterator: Iterator[A] = {
    def reverseList = {
      var curr: ListSetoid[A, Eq] = this
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
  private def addInternal(a: A): ListSetoid[A, Eq] = new Node(a)

  private def filterImpl(p: A => Boolean, isFlipped: Boolean): ListSetoid[A, Eq] = {
    var res = ListSetoid.empty[A, Eq]
    for (x <- this)
      if (p(x) != isFlipped) res = res.addInternal(x)
    res
  }
  override def filter(p: A => Boolean): ListSetoid[A, Eq] = filterImpl(p, isFlipped = false)
  override def filterNot(p: A => Boolean): ListSetoid[A, Eq] = filterImpl(p, isFlipped = true)

  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  protected def next: ListSetoid[A, Eq] = throw new NoSuchElementException("next of empty set")
  override def head: A = elem
  override def tail: ListSetoid[A, Eq] = next

  override def stringPrefix = "ListSet"

  /**
    * Represents an entry in the `ListSet`.
    */
  protected class Node(override protected val elem: A) extends ListSetoid[A, Eq] with Serializable {

    override def size = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(n: ListSetoid[A, Eq], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A)(implicit A: Equiv[A, Eq]) = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: ListSetoid[A, Eq], e: A)(implicit A: Equiv[A, Eq]): Boolean =
      !n.isEmpty && (A.equiv(n.elem, e) || containsInternal(n.next, e))

    override def +(e: A)(implicit A: Equiv[A, Eq]): ListSetoid[A, Eq] = if (contains(e)) this else new Node(e)

    override def -(e: A)(implicit A: Equiv[A, Eq]): ListSetoid[A, Eq] = removeInternal(e, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListSetoid[A, Eq], acc: List[ListSetoid[A, Eq]])(implicit A: Equiv[A, Eq]): ListSetoid[A, Eq] =
      if (cur.isEmpty) acc.last
      else if (A.equiv(k, cur.elem)) (cur.next /: acc) { case (t, h) => new t.Node(h.elem) }
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListSetoid[A, Eq] = ListSetoid.this

    override def last: A = elem
    override def init: ListSetoid[A, Eq] = next
  }
}
