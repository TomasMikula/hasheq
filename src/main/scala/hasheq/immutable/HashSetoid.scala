package hasheq
package immutable

import scala.annotation.tailrec

/** This class implements immutable sets using a hash trie.
 *
 *  @tparam A      the type of the elements contained in this hash set.
 *  @tparam Eq     equivalence relation on `A`
 */
sealed class HashSetoid[A, Eq <: Equiv[A]] extends Iterable[A]
{
  import HashSetoid.{nullToEmpty, bufferSize, LeafHashSetoid}

  override def size: Int = 0

  override def head: A = throw new NoSuchElementException("head of empty set")

  def iterator: Iterator[A] = Iterator.empty

  override def foreach[U](f: A => U): Unit = ()

  def contains(e: A)(implicit A: Hash[A, Eq], E: Eq): Boolean = get0(e, computeHash(e), 0)

  def subsetOf(that: HashSetoid[A, Eq])(implicit A: Hash[A, Eq], E: Eq): Boolean = subsetOf0(that, 0)

  /**
   * A specialized implementation of subsetOf for when both this and that are HashSet[A, Eq] and we can take advantage
   * of the tree structure of both operands and the precalculated hashcodes of the HashSet1 instances.
   * @param that the other set
   * @param level the level of this and that hashset
   *              The purpose of level is to keep track of how deep we are in the tree.
   *              We need this information for when we arrive at a leaf and have to call get0 on that
   *              The value of level is 0 for a top-level HashSet and grows in increments of 5
   * @return true if all elements of this set are contained in that set
   */
  protected def subsetOf0(that: HashSetoid[A, Eq], level: Int)(implicit E: Eq): Boolean = true // emtpy set is subset of any set

  def + (e: A)(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] = updated0(e, computeHash(e), 0)

  def + (elem1: A, elem2: A, elems: A*)(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] =
    this + elem1 + elem2 ++ elems

  def ++(that: Iterable[A])(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] =
    that.foldLeft(this)(_ + _)

  def union(that: HashSetoid[A, Eq])(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] = {
    val buffer = new Array[HashSetoid[A, Eq]](bufferSize(this.size + that.size))
    nullToEmpty(union0(that, 0, buffer, 0))
  }

  def intersect(that: HashSetoid[A, Eq])(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] = {
    val buffer = new Array[HashSetoid[A, Eq]](bufferSize(this.size min that.size))
    nullToEmpty(intersect0(that, 0, buffer, 0))
  }

  def diff(that: HashSetoid[A, Eq])(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] = {
    val buffer = new Array[HashSetoid[A, Eq]](bufferSize(this.size))
    nullToEmpty(diff0(that, 0, buffer, 0))
  }

  /**
   * Union with a leaf HashSet at a given level.
   * @param that a leaf HashSet
   * @param level the depth in the tree. We need this when we have to create a branch node on top of this and that
   * @return The union of this and that at the given level. Unless level is zero, the result is not a self-contained
   *         HashSet but needs to be stored at the correct depth
   */
  private[immutable] def union0(that: LeafHashSetoid[A, Eq], level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
    // the default implementation is for the empty set, so we just return that
    that
  }

  /**
   * Union with a HashSet at a given level
   * @param that a HashSet
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The union of this and that at the given level. Unless level is zero, the result is not a self-contained
   *         HashSet but needs to be stored at the correct depth
   */
  private[immutable] def union0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
    // the default implementation is for the empty set, so we just return that
    that
  }

  /**
   * Intersection with another hash set at a given level
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The intersection of this and that at the given level. Unless level is zero, the result is not a
   *         self-contained HashSet but needs to be stored at the correct depth
   */
  private[immutable] def intersect0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
    // the default implementation is for the empty set, so we just return the empty set
    null
  }

  /**
   * Diff with another hash set at a given level
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The diff of this and that at the given level. Unless level is zero, the result is not a
   *         self-contained HashSet but needs to be stored at the correct depth
   */
  private[immutable] def diff0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
    // the default implementation is for the empty set, so we just return the empty set
    null
  }

  def - (e: A)(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] =
    nullToEmpty(removed0(e, computeHash(e), 0))

  override def filter(p: A => Boolean) = {
    val buffer = new Array[HashSetoid[A, Eq]](bufferSize(size))
    nullToEmpty(filter0(p, false, 0, buffer, 0))
  }

  override def filterNot(p: A => Boolean) = {
    val buffer = new Array[HashSetoid[A, Eq]](bufferSize(size))
    nullToEmpty(filter0(p, true, 0, buffer, 0))
  }

  protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int): HashSetoid[A, Eq] = null

  protected def elemHashCode(key: A)(implicit A: Hash[A, Eq]) = A.hash(key)

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[hasheq] def computeHash(key: A)(implicit A: Hash[A, Eq]) = improve(elemHashCode(key))

  protected def get0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): Boolean = false

  private[hasheq] def updated0(key: A, hash: Int, level: Int)(implicit E: Eq): HashSetoid[A, Eq] =
    new HashSetoid.HashSetoid1(key, hash)

  protected def removed0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = this
}

object HashSetoid {

  implicit def setReprInstance[A, Eq <: Equiv[A]](implicit A: Hash[A, Eq]): Setoid[HashSetoid, A, Eq] = new Setoid[HashSetoid, A, Eq] {
    def empty: HashSetoid[A, Eq] = HashSetoid.empty[A, Eq]
    def size(s: HashSetoid[A, Eq]): Int = s.size
    def contains(s: HashSetoid[A, Eq], a: A)(implicit E: Eq): Boolean = s.contains(a)
    def iterator(s: HashSetoid[A, Eq]): Iterator[A] = s.iterator
    def add(s: HashSetoid[A, Eq], a: A)(implicit E: Eq): HashSetoid[A, Eq] = s + a
    def remove(s: HashSetoid[A, Eq], a: A)(implicit E: Eq): HashSetoid[A, Eq] = s - a
  }

  private object EmptyHashSetoid extends HashSetoid[Any, Equiv[Any]] {
    override def head: Any = throw new NoSuchElementException("Empty Set")
    override def tail: HashSetoid[Any, Equiv[Any]] = throw new NoSuchElementException("Empty Set")
  }
  private[hasheq] def emptyInstance: HashSetoid[Any, Equiv[Any]] = EmptyHashSetoid

  def empty[A, Eq <: Equiv[A]]: HashSetoid[A, Eq] = EmptyHashSetoid.asInstanceOf[HashSetoid[A, Eq]]
  def apply[A, Eq <: Equiv[A]](elems: A*)(implicit A: Hash[A, Eq], E: Eq): HashSetoid[A, Eq] = elems.foldLeft(empty[A, Eq])(_ + _)

  // utility method to create a HashTrieSet from two leaf HashSets (HashSet1 or HashSetCollision1) with non-colliding hash code)
  private def makeHashTrieSet[A, Eq <: Equiv[A]](hash0:Int, elem0:HashSetoid[A, Eq], hash1:Int, elem1:HashSetoid[A, Eq], level:Int) : HashTrieSetoid[A, Eq] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashSetoid[A, Eq]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieSetoid[A, Eq](bitmap, elems, elem0.size + elem1.size)
    } else {
      val elems = new Array[HashSetoid[A, Eq]](1)
      val bitmap = (1 << index0)
      val child = makeHashTrieSet(hash0, elem0, hash1, elem1, level + 5)
      elems(0) = child
      new HashTrieSetoid[A, Eq](bitmap, elems, child.size)
    }
  }

  /**
   * Common superclass of HashSet1 and HashSetCollision1, which are the two possible leaves of the Trie
   */
  private[HashSetoid] sealed abstract class LeafHashSetoid[A, Eq <: Equiv[A]] extends HashSetoid[A, Eq] {
    private[HashSetoid] def hash:Int
  }

  class HashSetoid1[A, Eq <: Equiv[A]](private[HashSetoid] val key: A, private[HashSetoid] val hash: Int) extends LeafHashSetoid[A, Eq] {
    override def size = 1

    override protected def get0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): Boolean =
      (hash == this.hash && A.equiv(key, this.key))

    override protected def subsetOf0(that: HashSetoid[A, Eq], level: Int)(implicit E: Eq) = {
      // check if that contains this.key
      // we use get0 with our key and hash at the correct level instead of calling contains,
      // which would not work since that might not be a top-level HashSet
      // and in any case would be inefficient because it would require recalculating the hash code
      that.get0(key, hash, level)
    }

    override private[hasheq] def updated0(key: A, hash: Int, level: Int)(implicit E: Eq): HashSetoid[A, Eq] =
      if (hash == this.hash && E.equiv(key, this.key)) this
      else {
        if (hash != this.hash) {
          makeHashTrieSet(this.hash, this, hash, new HashSetoid1(key, hash), level)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashSetoidCollision1(hash, ListSet.empty + this.key + key)
        }
      }

    override private[immutable] def union0(that: LeafHashSetoid[A, Eq], level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = that match {
      case that if that.hash != this.hash =>
        // different hash code, so there is no need to investigate further.
        // Just create a branch node containing the two.
        makeHashTrieSet(this.hash, this, that.hash, that, level)
      case that: HashSetoid1[A, Eq] =>
        if (A.equiv(this.key, that.key)) {
          this
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashSetoidCollision1[A, Eq](hash, ListSet.empty + this.key + that.key)
        }
      case that: HashSetoidCollision1[A, Eq] =>
        val ks1 = that.ks + key
        // Could use eq check (faster) if ListSet was guaranteed to return itself
        if (ks1.size == that.ks.size) {
          that
        } else {
          new HashSetoidCollision1[A, Eq](hash, ks1)
        }
    }

    override private[immutable] def union0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]) = {
      // switch to the Leaf version of union
      // we can exchange the arguments because union is symmetrical
      that.union0(this, level)
    }

    override private[immutable] def intersect0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] =
      if (that.get0(key, hash, level)) this else null

    override private[immutable] def diff0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] =
      if (that.get0(key, hash, level)) null else this

    override protected def removed0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] =
      if (hash == this.hash && A.equiv(key, this.key)) null else this

    override protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int): HashSetoid[A, Eq] =
      if (negate ^ p(key)) this else null

    override def iterator: Iterator[A] = Iterator(key)
    override def foreach[U](f: A => U): Unit = f(key)
  }

  private[immutable] class HashSetoidCollision1[A, Eq <: Equiv[A]](private[hasheq] val hash: Int, val ks: ListSet[A]) extends LeafHashSetoid[A, Eq] {

    override def size = ks.size

    override protected def get0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): Boolean =
      if (hash == this.hash) ks.contains(key) else false

    override protected def subsetOf0(that: HashSetoid[A, Eq], level: Int)(implicit E: Eq) = {
      // we have to check each element
      // we use get0 with our hash at the correct level instead of calling contains,
      // which would not work since that might not be a top-level HashSet
      // and in any case would be inefficient because it would require recalculating the hash code
      ks.forall(key => that.get0(key, hash, level))
    }

    override private[hasheq] def updated0(key: A, hash: Int, level: Int)(implicit E: Eq): HashSetoid[A, Eq] =
      if (hash == this.hash) new HashSetoidCollision1(hash, ks + key)
      else makeHashTrieSet(this.hash, this, hash, new HashSetoid1(key, hash), level)

    override private[immutable] def union0(that: LeafHashSetoid[A, Eq], level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = that match {
      case that if that.hash != this.hash =>
        // different hash code, so there is no need to investigate further.
        // Just create a branch node containing the two.
        makeHashTrieSet(this.hash, this, that.hash, that, level)
      case that: HashSetoid1[A, Eq] =>
        val ks1 = ks + that.key
        // Could use eq check (faster) if ListSet was guaranteed to return itself
        if (ks1.size == ks.size) {
          this
        } else {
          // create a new HashSetCollision with the existing hash
          // we don't have to check for size=1 because union is never going to remove elements
          new HashSetoidCollision1[A, Eq](hash, ks1)
        }
      case that: HashSetoidCollision1[A, Eq] =>
        val ks1 = this.ks ++ that.ks
        ks1.size match {
          case size if size == this.ks.size =>
            // could this check be made faster by doing an eq check?
            // I am not sure we can rely on ListSet returning itself when all elements are already in the set,
            // so it seems unwise to rely on it.
            this
          case size if size == that.ks.size =>
            // we have to check this as well, since we don't want to create a new instance if this is a subset of that
            that
          case _ =>
            // create a new HashSetCollision with the existing hash
            // we don't have to check for size=1 because union is never going to remove elements
            new HashSetoidCollision1[A, Eq](hash, ks1)
        }
    }

    override private[immutable] def union0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = that match {
      case that: LeafHashSetoid[A, Eq] =>
        // switch to the simpler Tree/Leaf implementation
        this.union0(that, level)
      case that: HashTrieSetoid[A, Eq] =>
        // switch to the simpler Tree/Leaf implementation
        // we can swap this and that because union is symmetrical
        that.union0(this, level)
      case _ => this
    }

    override private[immutable] def intersect0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
      // filter the keys, taking advantage of the fact that we know their hash code
      val ks1 = ks.filter(that.get0(_, hash, level))
      ks1.size match {
        case 0 =>
          // the empty set
          null
        case size if size == this.size =>
          // unchanged
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          this
        case size if size == that.size =>
          // the other set
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          that
        case 1 =>
          // create a new HashSet1 with the hash we already know
          new HashSetoid1(ks1.head, hash)
        case _ =>
          // create a new HashSetCollision with the hash we already know and the new keys
          new HashSetoidCollision1(hash, ks1)
      }
    }

    override private[immutable] def diff0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
      val ks1 = ks.filterNot(that.get0(_, hash, level))
      ks1.size match {
        case 0 =>
          // the empty set
          null
        case size if size == this.size =>
          // unchanged
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          this
        case 1 =>
          // create a new HashSet1 with the hash we already know
          new HashSetoid1(ks1.head, hash)
        case _ =>
          // create a new HashSetCollision with the hash we already know and the new keys
          new HashSetoidCollision1(hash, ks1)
      }
    }

    override protected def removed0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] =
      if (hash == this.hash) {
        val ks1 = ks - key
        ks1.size match {
          case 0 =>
            // the empty set
            null
          case 1 =>
            // create a new HashSet1 with the hash we already know
            new HashSetoid1(ks1.head, hash)
          case size if size == ks.size =>
            // Should only have HSC1 if size > 1
            this
          case _ =>
            // create a new HashSetCollision with the hash we already know and the new keys
            new HashSetoidCollision1(hash, ks1)
        }
      } else this

    override protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int): HashSetoid[A, Eq] = {
      val ks1 = if(negate) ks.filterNot(p) else ks.filter(p)
      ks1.size match {
        case 0 =>
          null
        case 1 =>
          new HashSetoid1(ks1.head, hash)
        case x if x == ks.size =>
          this
        case _ =>
          new HashSetoidCollision1(hash, ks1)
      }
    }

    override def iterator: Iterator[A] = ks.iterator
    override def foreach[U](f: A => U): Unit = ks.foreach(f)

  }

  /**
   * A branch node of the HashTrieSet with at least one and up to 32 children.
   *
   * @param bitmap encodes which element corresponds to which child
   * @param elems the up to 32 children of this node.
   *              the number of children must be identical to the number of 1 bits in bitmap
   * @param size0 the total number of elements. This is stored just for performance reasons.
   * @tparam A      the type of the elements contained in this hash set.
   *
   * How levels work:
   *
   * When looking up or adding elements, the part of the hashcode that is used to address the children array depends
   * on how deep we are in the tree. This is accomplished by having a level parameter in all internal methods
   * that starts at 0 and increases by 5 (32 = 2^5) every time we go deeper into the tree.
   *
   * hashcode (binary): 00000000000000000000000000000000
   * level=0 (depth=0)                             ^^^^^
   * level=5 (depth=1)                        ^^^^^
   * level=10 (depth=2)                  ^^^^^
   * ...
   *
   * Be careful: a non-toplevel HashTrieSet is not a self-contained set, so e.g. calling contains on it will not work!
   * It relies on its depth in the Trie for which part of a hash to use to address the children, but this information
   * (the level) is not stored due to storage efficiency reasons but has to be passed explicitly!
   *
   * How bitmap and elems correspond:
   *
   * A naive implementation of a HashTrieSet would always have an array of size 32 for children and leave the unused
   * children empty (null). But that would be very wasteful regarding memory. Instead, only non-empty children are
   * stored in elems, and the bitmap is used to encode which elem corresponds to which child bucket. The lowest 1 bit
   * corresponds to the first element, the second-lowest to the second, etc.
   *
   * bitmap (binary): 00010000000000000000100000000000
   * elems: [a,b]
   * children:        ---b----------------a-----------
   */
  class HashTrieSetoid[A, Eq <: Equiv[A]](private val bitmap: Int, private[hasheq] val elems: Array[HashSetoid[A, Eq]], private val size0: Int)
        extends HashSetoid[A, Eq] {
    assert(Integer.bitCount(bitmap) == elems.length)
    // assertion has to remain disabled until SI-6197 is solved
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))

    override def size = size0

    override protected def get0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): Boolean = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        elems(offset).get0(key, hash, level + 5)
      } else
        false
    }

    override private[hasheq] def updated0(key: A, hash: Int, level: Int)(implicit E: Eq): HashSetoid[A, Eq] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5)
        if (sub eq subNew) this
        else {
          val elemsNew = new Array[HashSetoid[A, Eq]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieSetoid(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[HashSetoid[A, Eq]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashSetoid1(key, hash)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieSetoid(bitmapNew, elemsNew, size + 1)
      }
    }

    override private[immutable] def union0(that: LeafHashSetoid[A, Eq], level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
      val index = (that.hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val sub1 = sub.union0(that, level + 5)
        if (sub eq sub1) this
        else {
          val elems1 = new Array[HashSetoid[A, Eq]](elems.length)
          Array.copy(elems, 0, elems1, 0, elems.length)
          elems1(offset) = sub1
          new HashTrieSetoid(bitmap, elems1, size + (sub1.size - sub.size))
        }
      } else {
        val elems1 = new Array[HashSetoid[A, Eq]](elems.length + 1)
        Array.copy(elems, 0, elems1, 0, offset)
        elems1(offset) = that
        Array.copy(elems, offset, elems1, offset + 1, elems.length - offset)
        val bitmap1 = bitmap | mask
        new HashTrieSetoid(bitmap1, elems1, size + that.size)
      }
    }

    override private[immutable] def union0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes.
        this
      case that: LeafHashSetoid[A, Eq] =>
        // when that is a leaf, we can switch to the simpler Tree/Leaf implementation
        this.union0(that, level)
      case that: HashTrieSetoid[A, Eq] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0

        // loop as long as there are bits left in either abm or bbm
        while ((abm | bbm) != 0) {
          // lowest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // lowest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).union0(b(bi), level + 5, buffer, offset)
            rs += sub1.size
            buffer(offset) = sub1
            offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb
            ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb
            bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            val sub1 = a(ai)
            rs += sub1.size
            buffer(offset) = sub1
            offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb
            ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            val sub1 = b(bi)
            rs += sub1.size
            buffer(offset) = sub1
            offset += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb
            bi += 1
          }
        }
        if (rs == this.size) {
          // if the result would be identical to this, we might as well return this
          this
        } else if (rs == that.size) {
          // if the result would be identical to that, we might as well return that
          that
        } else {
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          val length = offset - offset0
          val elems = new Array[HashSetoid[A, Eq]](length)
          System.arraycopy(buffer, offset0, elems, 0, length)
          new HashTrieSetoid(this.bitmap | that.bitmap, elems, rs)
        }
      case _ => this
    }

    override private[immutable] def intersect0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes!
        this
      case that: LeafHashSetoid[A, Eq] =>
        // when that is a leaf, we can switch to the simpler Tree/Leaf implementation
        // it is OK to swap the arguments because intersect is symmetric
        // (we can't do this in case of diff, which is not symmetric)
        that.intersect0(this, level, buffer, offset0)
      case that: HashTrieSetoid[A, Eq] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // if the bitmasks do not overlap, the result is definitely empty so we can abort here
        if ((abm & bbm) == 0)
          return null

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0
        var rbm = 0

        // loop as long as there are bits left that are set in both abm and bbm
        while ((abm & bbm) != 0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).intersect0(b(bi), level + 5, buffer, offset)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              buffer(offset) = sub1
              offset += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb;
            ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb;
            bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb;
            ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb;
            bi += 1
          }
        }

        if (rbm == 0) {
          // if the result bitmap is empty, the result is the empty set
          null
        } else if (rs == size0) {
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        } else if (rs == that.size0) {
          // if the result has the same number of elements as that, it must be identical to that,
          // so we might as well return that
          that
        } else {
          val length = offset - offset0
          if (length == 1 && !buffer(offset0).isInstanceOf[HashTrieSetoid[A, Eq]])
            buffer(offset0)
          else {
            val elems = new Array[HashSetoid[A, Eq]](length)
            System.arraycopy(buffer, offset0, elems, 0, length)
            new HashTrieSetoid[A, Eq](rbm, elems, rs)
          }
        }
      case _ => null
    }

    override private[immutable] def diff0(that: HashSetoid[A, Eq], level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes!
        null
      case that: HashSetoid1[A, Eq] =>
        removed0(that.key, that.hash, level)
      case that: HashTrieSetoid[A, Eq] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0
        var rbm = 0

        // loop until there are no more bits in abm
        while(abm!=0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).diff0(b(bi), level + 5, buffer, offset)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              buffer(offset) = sub1
              offset += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            val sub1 = a(ai)
            rs += sub1.size
            rbm |= alsb
            buffer(offset) = sub1; offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          }
        }
        if (rbm == 0) {
          null
        } else if (rs == this.size0) {
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        } else {
          val length = offset - offset0
          if (length == 1 && !buffer(offset0).isInstanceOf[HashTrieSetoid[A, Eq]])
            buffer(offset0)
          else {
            val elems = new Array[HashSetoid[A, Eq]](length)
            System.arraycopy(buffer, offset0, elems, 0, length)
            new HashTrieSetoid[A, Eq](rbm, elems, rs)
          }
        }
      case that: HashSetoidCollision1[A, Eq] =>
        // we remove the elements using removed0 so we can use the fact that we know the hash of all elements
        // to be removed
        @tailrec def removeAll(s:HashSetoid[A, Eq], r:ListSet[A]) : HashSetoid[A, Eq] =
          if(r.isEmpty || (s eq null)) s
          else removeAll(s.removed0(r.head, that.hash, level), r.tail)
        removeAll(this, that.ks)
      case _ => this
    }

    override protected def removed0(key: A, hash: Int, level: Int)(implicit A: Equiv[A]): HashSetoid[A, Eq] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (sub eq subNew) this
        else if (subNew eq null) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashSetoid[A, Eq]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieSetoid[_, _]])
              elemsNew(0)
            else
              new HashTrieSetoid(bitmapNew, elemsNew, sizeNew)
          } else
            null
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieSetoid[_, _]]) {
          subNew
        } else {
          val elemsNew = new Array[HashSetoid[A, Eq]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieSetoid(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override protected def subsetOf0(that: HashSetoid[A, Eq], level: Int)(implicit E: Eq): Boolean = if (that eq this) true else that match {
      case that: HashTrieSetoid[A, Eq] if this.size0 <= that.size0 =>
        // create local mutable copies of members
        var abm = this.bitmap
        val a = this.elems
        var ai = 0
        val b = that.elems
        var bbm = that.bitmap
        var bi = 0
        if ((abm & bbm) == abm) {
          // I tried rewriting this using tail recursion, but the generated java byte code was less than optimal
          while(abm!=0) {
            // highest remaining bit in abm
            val alsb = abm ^ (abm & (abm - 1))
            // highest remaining bit in bbm
            val blsb = bbm ^ (bbm & (bbm - 1))
            // if both trees have a bit set at the same position, we need to check the subtrees
            if (alsb == blsb) {
              // we are doing a comparison of a child of this with a child of that,
              // so we have to increase the level by 5 to keep track of how deep we are in the tree
              if (!a(ai).subsetOf0(b(bi), level + 5))
                return false
              // clear lowest remaining one bit in abm and increase the a index
              abm &= ~alsb; ai += 1
            }
            // clear lowermost remaining one bit in bbm and increase the b index
            // we must do this in any case
            bbm &= ~blsb; bi += 1
          }
          true
        } else {
          // the bitmap of this contains more one bits than the bitmap of that,
          // so this can not possibly be a subset of that
          false
        }
      case _ =>
        // if the other set is a HashTrieSet but has less elements than this, it can not be a subset
        // if the other set is a HashSet1, we can not be a subset of it because we are a HashTrieSet with at least two children (see assertion)
        // if the other set is a HashSetCollision1, we can not be a subset of it because we are a HashTrieSet with at least two different hash codes
        // if the other set is the empty set, we are not a subset of it because we are not empty
        false
    }

    override protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSetoid[A, Eq]], offset0: Int): HashSetoid[A, Eq] = {
      // current offset
      var offset = offset0
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p, negate, level + 5, buffer, offset)
        if (result ne null) {
          buffer(offset) = result
          offset += 1
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (offset == offset0) {
        // empty
        null
      } else if (rs == size0) {
        // unchanged
        this
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieSetoid[A, Eq]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieSet
        val length = offset - offset0
        val elems1 = new Array[HashSetoid[A, Eq]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieSetoid(bitmap1, elems1, rs)
      }
    }

    override def iterator = new TrieIterator[A](elems.asInstanceOf[Array[Iterable[A]]]) {
      final override def getElem(cc: AnyRef): A = cc.asInstanceOf[HashSetoid1[A, Eq]].key
    }

    override def foreach[U](f: A => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }
  }

  /**
   * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
   * @param size the maximum size of the collection to be generated
   * @return the maximum buffer size
   */
  @inline private def bufferSize(size: Int): Int = (size + 6) min (32 * 7)

  /**
   * In many internal operations the empty set is represented as null for performance reasons. This method converts
   * null to the empty set for use in public methods
   */
  @inline private def nullToEmpty[A, Eq <: Equiv[A]](s: HashSetoid[A, Eq]): HashSetoid[A, Eq] = if (s eq null) empty[A, Eq] else s

  /**
   * Utility method to keep a subset of all bits in a given bitmap
   *
   * Example
   *    bitmap (binary): 00000001000000010000000100000001
   *    keep (binary):                               1010
   *    result (binary): 00000001000000000000000100000000
   *
   * @param bitmap the bitmap
   * @param keep a bitmask containing which bits to keep
   * @return the original bitmap with all bits where keep is not 1 set to 0
   */
  private def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

  // unsigned comparison
  @inline private[this] def unsignedCompare(i: Int, j: Int) =
    (i < j) ^ (i < 0) ^ (j < 0)

}

