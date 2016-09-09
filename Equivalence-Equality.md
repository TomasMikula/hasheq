# Equivalence versus Equality

This page describes what we mean when we say that the data structures in this library are _equivalence-aware_ in a _type-safe_ fashion.


_Set_ is a data structure that doesn't contain _duplicate_ elements. An implementation of _Set_ must therefore have a way to compare elements for _"sameness"_. A useful notion of sameness is **equivalence**, i.e. a binary relation that is _reflexive_, _symmetric_ and _transitive_. Any sane implementation of _Set_ is equiped with _some_ equivalence relation on its element type.

Here's the catch: For any type with more than 1 inhabitant there are _multiple_ valid equivalence relations. We cannot (in general) pick one that is suitable in all contexts. For example, are these two binary trees _same_?

```
  +            +
 / \          / \
1   +        +   3
   / \      / \
  2   3    1   2
```

Depends on the context. They clearly have different structure, but they are both binary search trees containing the same elements. For a balancing algorithm, they are different trees, but as an implementation of _Set_, they represent the same set of integers.

Despite the non-uniqueness, there is one equivalence relation that stands out: **equality**. Two objects are considered _equal_ when they are _indistinguishable_ to an observer. Formally, equality is required to have the _substitution property:_

  - ∀ a,b ∈ A, ∀ f ∈ (A -> B): &nbsp;&nbsp; a=<sub>A</sub>b ⟹ f(a)=<sub>B</sub>f(b)

(Here, =<sub>A</sub> denotes equality on A, =<sub>B</sub> denotes equality on B.)
Equality is the finest equivalence: whenever two elements are _equal_, they are necessarily _equivalent_ with respect to every equivalence.

Popular Scala libraries take one of two approaches when dealing with comparing elements for _"sameness"_: they require either

 1. **Equality.** This is the approach currently taken by [cats](https://github.com/typelevel/cats/). Instances of the `cats.Eq[A]` typeclass are required to have all the properties of equality, including the substitution property above. The problem with this approach is that for some types, such as `Set[Int]`, equality is too strict to be useful:
  - Are values `Set(1, 2)` and `Set(2, 1)` _equal_? For that to be true, they have to be indistinguishable by any function. Let's try `(_.toList)`:

      ```scala
      scala> Set(1, 2).toList == Set(2, 1).toList
      res0: Boolean = false
      ```

    So, `Set(1, 2)` and `Set(2, 1)` are clearly _not_ equal. As a result, we cannot use `Set[Int]` in a context where equality is required (without cheating).

 2. **Further unspecified equivalence.** This is basically the current approach of [scalaz](https://github.com/scalaz/scalaz/). Although the name `scalaz.Equal[A]` might suggest _equality_, instances of this typeclass are only tested for properties of _equivalence_. As mentioned above, there are multiple _valid_ equivalence relations for virtually any type. When there are also multiple _useful_ equivalences for a type, we are at risk of mixing them up (and the fact that they are usually resolved as implicit arguments only makes things worse).


## Equivalence-aware sets (a.k.a. setoids)

Let's look at how _we_ deal with this issue. We define typeclass `Equiv` with an extra type parameter that serves as a _"tag"_ identifying the meaning of the equivalence.

```scala
trait Equiv[A, Eq] {
  def equiv(a: A, b: A): Boolean
}
```

For the compiler, the "tag" is an opaque type. It only has specific meaning for humans. The only meaning it has for the compiler is that different tags represent different equivalence relations.

An _equivalence-aware_ data structure then carries the tag of the equivalence it uses in its _type_.

```scala
import hasheq._
import hasheq.immutable._
import hasheq.std.int._
```

```scala
scala> HashSet(1, 2, 3, 4, 5)
res0: hasheq.immutable.HashSet[Int] = HashSetoid(5, 1, 2, 3, 4)
```

What on earth is `HashSetoid`? [_Setoid_](https://en.wikipedia.org/wiki/Setoid) is an _equivalence-aware set_. `HashSetoid` is then a setoid implementated using hash-table. Let's look at the definition of `HashSet`:

```scala
type HashSet[A] = HashSetoid[A, Equality.type]
```

So `HashSet` is just a `HashSetoid` whose equivalence is _equality_. To create an instance of `HashSet[Int]`, we needed to have an implicit instance of `Equiv[Int, Equality.type]` in scope.

```scala
scala> implicitly[Equiv[Int, Equality.type]]
res1: hasheq.Equiv[Int,hasheq.Equality.type] = hasheq.std.int$$anon$1@74009ae1
```

For the compiler, `Equality` is just a rather arbitrary singleton object. It only has the meaning of mathematical _equality_ for us.

There is a type alias

```scala
type Equal[A] = Equiv[A, Equality.type]
```

```scala
scala> implicitly[Equal[Int]]
res2: hasheq.Equal[Int] = hasheq.std.int$$anon$1@10c2ae97
```

So how do we deal with the problem of set equality mentioned above?

```scala
scala> implicitly[Equal[HashSet[Int]]]
<console>:22: error: could not find implicit value for parameter e: hasheq.Equal[hasheq.immutable.HashSet[Int]]
       implicitly[Equal[HashSet[Int]]]
                 ^
```

There is no equality defined for `HashSet[Int]`. That means we cannot have a `HashSet[HashSet[Int]]` (remember, for a `HashSet[A]`, we need an instance of `Equal[A]`).

```scala
scala> HashSet(HashSet(1, 2, 3, 4, 5))
<console>:22: error: could not find implicit value for parameter A: hasheq.Hash[hasheq.immutable.HashSet[Int]]
       HashSet(HashSet(1, 2, 3, 4, 5))
              ^
```

But we can have a `HashSetoid[HashSet[Int], _]`, for some equivalence on `HashSet[Int]`.

```scala
scala> HashSet.of(HashSet(1, 2, 3, 4, 5))
res5: hasheq.immutable.HashSetoid[hasheq.immutable.HashSet[Int],hasheq.immutable.Setoid.ContentEquiv[Int,hasheq.Equality.type]] = HashSetoid(HashSetoid(5, 1, 2, 3, 4))
```

`HashSet.of(elems)` is like `HashSet(elems)`, except it tries to infer the equivalence on the element type, instead of requiring it to be equality.

Notice the _equivalence tag_: `Setoid.ContentEquiv[Int, Equality.type]`. Its meaning is (again, for humans only) that two setoids are equivalent when they contain the same elements (here, of type `Int`), as compared by the given equivalence of elements (here, `Equality`).

The remaining question is: How does this work in the presence of _multiple useful equivalences?_

Let's define another equivalence on `Int` (in addition to the provided equality).

```scala
// Our "tag" for equivalence modulo 10.
// This trait will never be instantiated.
sealed trait Mod10

// Provide equivalence tagged by Mod10.
implicit object EqMod10 extends Equiv[Int, Mod10] {
  def mod10(i: Int): Int = {
    val r = i % 10
    if (r < 0) r + 10
    else r
  }
  def equiv(a: Int, b: Int): Boolean = mod10(a) == mod10(b)
}

// Provide hash function compatible with equivalence modulo 10.
// Note that the HashEq typeclass is also tagged by Mod10.
implicit object HashMod10 extends HashEq[Int, Mod10] {
  def hash(a: Int): Int = EqMod10.mod10(a)
}
```

Now let's create a "setoid of sets of integers", as before.

```scala
scala> HashSet.of(HashSet(1, 2, 3, 4, 5))
res13: hasheq.immutable.HashSetoid[hasheq.immutable.HashSet[Int],hasheq.immutable.Setoid.ContentEquiv[Int,hasheq.Equality.type]] = HashSetoid(HashSetoid(5, 1, 2, 3, 4))
```

This still works, because `HashSet` requires an equality on `Int`, an there is only one in the implicit scope. Let's try to create a "setoid of setoids of integers":

```scala
scala> HashSet.of(HashSet.of(1, 2, 3, 4, 5))
<console>:25: error: ambiguous implicit values:
 both method hashInstance in object int of type => hasheq.Hash[Int]
 and object HashMod10 of type HashMod10.type
 match expected type hasheq.HashEq[Int,Eq]
       HashSet.of(HashSet.of(1, 2, 3, 4, 5))
                            ^
```

This fails, because there are more equivalences on `Int` in scope. We need to be more specific:

```scala
scala> HashSet.of(HashSet.of[Int, Mod10](1, 2, 3, 4, 5))
res15: hasheq.immutable.HashSetoid[hasheq.immutable.HashSetoid[Int,Mod10],hasheq.immutable.Setoid.ContentEquiv[Int,Mod10]] = HashSetoid(HashSetoid(5, 1, 2, 3, 4))
```

Finally, does it **prevent mixing up equivalences**? Let's see:

```scala
scala> val s1 = HashSet(1,  2,  3,         11, 12, 13    )
s1: hasheq.immutable.HashSet[Int] = HashSetoid(1, 13, 2, 12, 3, 11)

scala> val s2 = HashSet(    2,  3,  4,  5,         13, 14)
s2: hasheq.immutable.HashSet[Int] = HashSetoid(5, 14, 13, 2, 3, 4)

scala> val t1 = HashSet.of[Int, Mod10](1,  2,  3,         11, 12, 13    )
t1: hasheq.immutable.HashSetoid[Int,Mod10] = HashSetoid(1, 2, 3)

scala> val t2 = HashSet.of[Int, Mod10](    2,  3,  4,  5,         13, 14)
t2: hasheq.immutable.HashSetoid[Int,Mod10] = HashSetoid(5, 2, 3, 4)
```

Combining compatible setoids:

```scala
scala> s1 union s2
res16: hasheq.immutable.HashSetoid[Int,hasheq.Equality.type] = HashSetoid(5, 14, 1, 13, 2, 12, 3, 11, 4)

scala> t1 union t2
res17: hasheq.immutable.HashSetoid[Int,Mod10] = HashSetoid(5, 1, 2, 3, 4)
```

Combining incompatible setoids:

```scala
scala> s1 union t2
<console>:27: error: type mismatch;
 found   : hasheq.immutable.HashSetoid[Int,Mod10]
 required: hasheq.immutable.HashSetoid[Int,hasheq.Equality.type]
       s1 union t2
                ^

scala> t1 union s2
<console>:27: error: type mismatch;
 found   : hasheq.immutable.HashSet[Int]
    (which expands to)  hasheq.immutable.HashSetoid[Int,hasheq.Equality.type]
 required: hasheq.immutable.HashSetoid[Int,Mod10]
       t1 union s2
                ^
```


## Conclusion

We went one step further in the direction of type-safe equivalence in Scala compared to what is typically seen out in the wild today. There is nothing very sophisticated about this encoding. I think the major win is that we can design APIs so that the extra type parameter (the "equivalence tag") stays unnoticed by the user of the API as long as they only deal with _equivalences_. As soon as it starts requesting our attention (via an ambiguous implicit or a type error), it is likely that the attention is justified.
