package hasheq
package immutable

import scala.annotation.unchecked.{ uncheckedVariance=> uV }

/** This class implements immutable maps using a hash trie.
 *
 *  @tparam A      the type of the keys contained in this hash map.
 *  @tparam B      the type of the values associated with the keys.
 *
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#hash_tries "Scala's Collection Library overview"]]
 *  section on `Hash Tries` for more information.
 */
sealed class HashMap[A, +B] extends Iterable[(A, B)]
{
  import HashMap.{nullToEmpty, bufferSize}

  override def size: Int = 0

  override def isEmpty: Boolean = size == 0

  override def head: (A, B) = throw new NoSuchElementException("head of empty map")

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) => U): Unit = ()

  def get(key: A)(implicit A: HashEq[A]): Option[B] =
    get0(key, computeHash(key), 0)

  def put[B1 >: B](key: A, value: B1)(implicit A: HashEq[A]): HashMap[A, B1] =
    updated0(key, computeHash(key), 0, value, null, null)

  def updated[B1 >: B](key: A, value: B1)(combine: (B1, B1) => B1)(implicit A: HashEq[A]): HashMap[A, B1] = {
    get(key) match {
      case Some(v0) => put(key, combine(v0, value))
      case None     => put(key, value)
    }
  }

  def + [B1 >: B] (kv: (A, B1))(implicit A: HashEq[A]): HashMap[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv, null)

  def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *)(implicit A: HashEq[A]): HashMap[A, B1] =
    this + elem1 + elem2 ++ elems

  def ++[B1 >: B](elems: Iterable[(A, B1)])(implicit A: HashEq[A]): HashMap[A, B1] =
    elems.foldLeft(this: HashMap[A, B1])(_ + _)

  def - (key: A)(implicit A: HashEq[A]): HashMap[A, B] =
    removed0(key, computeHash(key), 0)

  override def filter(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, false, 0, buffer, 0))
  }

  override def filterNot(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, true, 0, buffer, 0))
  }

  protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = null

  protected def elemHashCode(key: A)(implicit A: HashEq[A]): Int = A.hash(key)

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[hasheq] def computeHash(key: A)(implicit A: HashEq[A]) = improve(elemHashCode(key))

  import HashMap.{Merger, MergeFunction, liftMerger}

  private[hasheq] def get0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): Option[B] = None

  private[hasheq] def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] =
    new HashMap.HashMap1(key, hash, value, kv)

  protected def removed0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): HashMap[A, B] = this

  def split: Seq[HashMap[A, B]] = Seq(this)

  /** Creates a new map which is the merge of this and the argument hash map.
   *
   *  Uses the specified collision resolution function if two keys are the same.
   *  The collision resolution function will always take the first argument from
   *  `this` hash map and the second from `that`.
   *
   *  The `merged` method is on average more performant than doing a traversal and reconstructing a
   *  new immutable hash map from scratch, or `++`.
   *
   *  @tparam B1      the value type of the other hash map
   *  @param that     the other hash map
   *  @param mergef   the merge function or null if the first key-value pair is to be picked
   */
  def merged[B1 >: B](that: HashMap[A, B1])(mergef: MergeFunction[A, B1])(implicit A: Eq[A]): HashMap[A, B1] = merge0(that, 0, liftMerger(mergef))

  protected def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] = that

}

object HashMap {

  implicit def mapReprInstance[K: HashEq]: MapRepr[HashMap, K] = new MapRepr[HashMap, K] {
    def empty[V]: HashMap[K, V] = HashMap.empty[K, V]
    def get[V](m: HashMap[K, V], k: K): Option[V] = m.get(k)
    def iterator[V](m: HashMap[K, V]): Iterator[(K, V)] = m.iterator
    def put[V](m: HashMap[K, V], k: K, v: V): HashMap[K, V] = m.put(k, v)
    def size[V](m: HashMap[K, V]): Int = m.size

    override def entries[V](m: HashMap[K, V]): Iterable[(K, V)] = m
    override def updated[V](m: HashMap[K, V], k: K, v: V)(combine: (V, V) => V): HashMap[K, V] = m.updated(k, v)(combine)
  }

  private[hasheq] abstract class Merger[A, B] {
    def apply(kv1: (A, B), kv2: (A, B)): (A, B)
    def flip: Merger[A, B]
  }

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  private def liftMerger[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] =
    if (mergef == null) defaultMerger.asInstanceOf[Merger[A1, B1]] else liftMerger0(mergef)

  private[this] val defaultMerger : Merger[Any, Any] = liftMerger0((a,b) => a)

  private[this] def liftMerger0[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] = new Merger[A1, B1] {
    self =>
    def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv1, kv2)
    val flip: Merger[A1, B1] = new Merger[A1, B1] {
      def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv2, kv1)
      def flip: Merger[A1, B1] = self
    }
  }

  private object EmptyHashMap extends HashMap[Any, Nothing] { 
    override def head: (Any, Nothing) = throw new NoSuchElementException("Empty Map")
    override def tail: HashMap[Any, Nothing] = throw new NoSuchElementException("Empty Map")
  }

  def empty[K, V]: HashMap[K, V] = EmptyHashMap.asInstanceOf[HashMap[K, V]]

  def apply[A, B](elems: (A, B)*)(implicit A: HashEq[A]): HashMap[A, B] =
    elems.foldLeft(empty[A, B])(_ + _)

  // utility method to create a HashTrieMap from two leaf HashMaps (HashMap1 or HashMapCollision1) with non-colliding hash code)
  private def makeHashTrieMap[A, B](hash0:Int, elem0:HashMap[A, B], hash1:Int, elem1:HashMap[A, B], level:Int, size:Int)(implicit A: Eq[A]) : HashTrieMap[A, B] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashMap[A,B]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieMap[A, B](bitmap, elems, size)
    } else {
      val elems = new Array[HashMap[A,B]](1)
      val bitmap = (1 << index0)
      elems(0) = makeHashTrieMap(hash0, elem0, hash1, elem1, level + 5, size)
      new HashTrieMap[A, B](bitmap, elems, size)
    }
  }

  class HashMap1[A,+B](private[hasheq] val key: A, private[hasheq] val hash: Int, private[hasheq] val value: (B @uV), private[hasheq] var kv: (A,B @uV)) extends HashMap[A,B] {
    override def size = 1

    private[hasheq] def getKey = key
    private[hasheq] def getHash = hash
    private[hasheq] def computeHashFor(k: A)(implicit A: HashEq[A]) = computeHash(k)

    override def get0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): Option[B] =
      if (hash == this.hash && A.equal(key, this.key)) Some(value) else None

    private[hasheq] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] =
      if (hash == this.hash && A.equal(key, this.key)) {
        if (merger eq null) {
          if (this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this
          else new HashMap1(key, hash, value, kv)
        } else {
          val nkv = merger(this.ensurePair, if(kv != null) kv else (key, value))
          new HashMap1(nkv._1, hash, nkv._2, nkv)
        }
      } else {
        if (hash != this.hash) {
          // they have different hashes, but may collide at this level - find a level at which they don't
          val that = new HashMap1[A, B1](key, hash, value, kv)
          makeHashTrieMap[A,B1](this.hash, this, hash, that, level, 2)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
        }
      }

    override def removed0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): HashMap[A, B] =
      if (hash == this.hash && A.equal(key, this.key)) HashMap.empty[A,B] else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] =
      if (negate ^ p(ensurePair)) this else null

    override def iterator: Iterator[(A,B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    // this method may be called multiple times in a multithreaded environment, but that's ok
    private[HashMap] def ensurePair: (A,B) = if (kv ne null) kv else { kv = (key, value); kv }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] = {
      that.updated0(key, hash, level, value, kv, merger.flip)
    }
  }

  private[hasheq] class HashMapCollision1[A, +B](private[hasheq] val hash: Int, val kvs: ListMap[A, B @uV])
          extends HashMap[A, B @uV] {
    // assert(kvs.size > 1)

    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    private[hasheq] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] =
      if (hash == this.hash) {
        if ((merger eq null) || !kvs.contains(key)) new HashMapCollision1(hash, kvs.updated(key, value))
        else new HashMapCollision1(hash, kvs + merger((key, kvs(key)), kv))
      } else {
        val that = new HashMap1(key, hash, value, kv)
        makeHashTrieMap(this.hash, this, hash, that, level, size + 1)
      }

    override def removed0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): HashMap[A, B] =
      if (hash == this.hash) {
        val kvs1 = kvs - key
        kvs1.size match {
          case 0 =>
            HashMap.empty[A,B]
          case 1 =>
            val kv = kvs1.head
            new HashMap1(kv._1,hash,kv._2,kv)
          case x if x == kvs.size =>
            this
          case _ =>
            new HashMapCollision1(hash, kvs1)
        }
      } else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = {
      val kvs1 = if(negate) kvs.filterNot(p) else kvs.filter(p)
      kvs1.size match {
        case 0 =>
          null
        case 1 =>
          val kv@(k,v) = kvs1.head
          new HashMap1(k, hash, v, kv)
        case x if x == kvs.size =>
          this
        case _ =>
          new HashMapCollision1(hash, kvs1)
      }
    }

    override def iterator: Iterator[(A,B)] = kvs.iterator
    override def foreach[U](f: ((A, B)) => U): Unit = kvs.foreach(f)
    override def split: Seq[HashMap[A, B]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[A, B @uV]) = new HashMapCollision1(hash, lm)
      List(newhm(x), newhm(y))
    }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] = {
      // this can be made more efficient by passing the entire ListMap at once
      var m = that
      for (p <- kvs) m = m.updated0(p._1, this.hash, level, p._2, p, merger)
      m
    }
  }

  class HashTrieMap[A, +B](
    private[hasheq] val bitmap: Int,
    private[hasheq] val elems: Array[HashMap[A, B @uV]],
    private[hasheq] val size0: Int
  ) extends HashMap[A, B @uV] {

    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieMap[_,_]]))

    override def size = size0

    override def get0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): Option[B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        elems(offset).get0(key, hash, level + 5)
      } else
        None
    }

    private[hasheq] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
        if(subNew eq sub) this else {
          val elemsNew = new Array[HashMap[A,B1]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashMap1(key, hash, value, kv)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        new HashTrieMap(bitmap | mask, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int)(implicit A: Eq[A]): HashMap[A, B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew eq sub) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashMap[A,B]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieMap[_,_]])
              elemsNew(0)
            else
              new HashTrieMap(bitmapNew, elemsNew, sizeNew)
          } else
            HashMap.empty[A,B]
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieMap[_,_]]) {
          subNew
        } else {
          val elemsNew = new Array[HashMap[A,B]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = {
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
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieMap[A, B]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieMap
        val length = offset - offset0
        val elems1 = new Array[HashMap[A, B]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieMap(bitmap1, elems1, rs)
      }
    }

    override def iterator: Iterator[(A, B)] = new TrieIterator[(A, B)](elems.asInstanceOf[Array[Iterable[(A, B)]]]) {
      final override def getElem(cc: AnyRef): (A, B) = cc.asInstanceOf[HashMap1[A, B]].ensurePair
    }

    override def foreach[U](f: ((A, B)) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    private def posOf(n: Int, bm: Int) = {
      var left = n
      var i = -1
      var b = bm
      while (left >= 0) {
        i += 1
        if ((b & 1) != 0) left -= 1
        b = b >>> 1
      }
      i
    }

    override def split: Seq[HashMap[A, B]] = if (size == 1) Seq(this) else {
      val nodesize = Integer.bitCount(bitmap)
      if (nodesize > 1) {
        val splitpoint = nodesize / 2
        val bitsplitpoint = posOf(nodesize / 2, bitmap)
        val bm1 = bitmap & (-1 << bitsplitpoint)
        val bm2 = bitmap & (-1 >>> (32 - bitsplitpoint))

        val (e1, e2) = elems.splitAt(splitpoint)
        val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
        val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

        List(hm1, hm2)
      } else elems(0).split
    }

    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1])(implicit A: Eq[A]): HashMap[A, B1] = that match {
      case hm: HashMap1[_, _] =>
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[B1], hm.kv, merger)
      case hm: HashTrieMap[_, _] =>
        val that = hm.asInstanceOf[HashTrieMap[A, B1]]
        val thiselems = this.elems
        val thatelems = that.elems
        var thisbm = this.bitmap
        var thatbm = that.bitmap

        // determine the necessary size for the array
        val subcount = Integer.bitCount(thisbm | thatbm)

        // construct a new array of appropriate size
        val merged = new Array[HashMap[A, B1]](subcount)

        // run through both bitmaps and add elements to it
        var i = 0
        var thisi = 0
        var thati = 0
        var totalelems = 0
        while (i < subcount) {
          val thislsb = thisbm ^ (thisbm & (thisbm - 1))
          val thatlsb = thatbm ^ (thatbm & (thatbm - 1))

          // collision
          if (thislsb == thatlsb) {
            val m = thiselems(thisi).merge0(thatelems(thati), level + 5, merger)
            totalelems += m.size
            merged(i) = m
            thisbm = thisbm & ~thislsb
            thatbm = thatbm & ~thatlsb
            thati += 1
            thisi += 1
          } else {
            // condition below is due to 2 things:
            // 1) no unsigned int compare on JVM
            // 2) 0 (no lsb) should always be greater in comparison
            if (BitOperations.Int.unsignedCompare(thislsb - 1, thatlsb - 1)) {
              val m = thiselems(thisi)
              totalelems += m.size
              merged(i) = m
              thisbm = thisbm & ~thislsb
              thisi += 1
            }
            else {
              val m = thatelems(thati)
              totalelems += m.size
              merged(i) = m
              thatbm = thatbm & ~thatlsb
              thati += 1
            }
          }
          i += 1
        }

        new HashTrieMap[A, B1](this.bitmap | that.bitmap, merged, totalelems)
      case hm: HashMapCollision1[_, _] => that.merge0(this, level, merger.flip)
      case hm: HashMap[_, _] => this
      case _ => sys.error("section supposed to be unreachable.")
    }
  }

  /**
   * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
   * @param size the maximum size of the collection to be generated
   * @return the maximum buffer size
   */
  @inline private def bufferSize(size: Int): Int = (size + 6) min (32 * 7)

  /**
   * In many internal operations the empty map is represented as null for performance reasons. This method converts
   * null to the empty map for use in public methods
   */
  @inline private def nullToEmpty[A, B](m: HashMap[A, B]): HashMap[A, B] = if (m eq null) empty[A, B] else m

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
}
