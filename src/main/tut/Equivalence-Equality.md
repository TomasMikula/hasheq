# Equivalence versus Equality

This page describes what we mean when we say that the data structures in this library are _equivalence-aware_ in a _type-safe_ fashion.


_Set_ is a data structure that doesn't contain _duplicate_ elements. An implementation of _Set_ must therefore have a way to compare elements for _"sameness"_. A useful notion of sameness is **equivalence**, i.e. a binary relation that is _reflexive_, _symmetric_ and _transitive_. Any sane implementation of _Set_ is equiped with _some_ equivalence relation on its element type.

**Here's the catch:** For any type with more than 1 inhabitant there are _multiple_ valid equivalence relations. We cannot (in general) pick one that is suitable in all contexts. For example, are these two binary trees _same_?

```
  +            +
 / \          / \
1   +        +   3
   / \      / \
  2   3    1   2
```

It depends on the context. They clearly have different structure, but they are both binary search trees containing the same elements. For a balancing algorithm, they are different trees, but as an implementation of _Set_, they represent the same set of integers.

Despite the non-uniqueness, there is one equivalence relation that stands out: **equality**. Two objects are considered _equal_ when they are _indistinguishable_ to an observer. Formally, equality is required to have the _substitution property:_

  - ∀ a,b ∈ A, ∀ f ∈ (A -> B): &nbsp;&nbsp; a=<sub>A</sub>b ⟹ f(a)=<sub>B</sub>f(b)

(Here, =<sub>A</sub> denotes equality on A, =<sub>B</sub> denotes equality on B.)
Equality is the finest equivalence: whenever two elements are _equal_, they are necessarily _equivalent_ with respect to every equivalence.

Popular Scala libraries take one of these two approaches when dealing with comparing elements for _"sameness"_: they require either

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

For the compiler, the "tag" is an opaque type. It only has specific meaning for humans. The only meaning it has for the compiler is that different tags represent (intensionally) different equivalence relations.

An _equivalence-aware_ data structure then carries in its _type_ the tag of the equivalence it uses.

```tut:silent
import hasheq._
import hasheq.immutable._
import hasheq.std.int._
```

```tut
HashSet(1, 2, 3, 4, 5)
```

What on earth is `HashSetoid`? [_Setoid_](https://en.wikipedia.org/wiki/Setoid) is an _equivalence-aware set_. `HashSetoid` is then just a setoid implementated using hash-table. Let's look at the definition of `HashSet`:

```scala
type HashSet[A] = HashSetoid[A, Equality.type]
```

So `HashSet` is just a `HashSetoid` whose equivalence is _equality_. To create an instance of `HashSet[Int]` above, we needed to have an implicit instance of `Equiv[Int, Equality.type]` in scope.

```tut
implicitly[Equiv[Int, Equality.type]]
```

For the compiler, `Equality` is just a rather arbitrary singleton object. It only has the meaning of mathematical _equality_ for us, humans.

There is a convenient type alias provided for _equality_ relation:

```scala
type Equal[A] = Equiv[A, Equality.type]
```

```tut
implicitly[Equal[Int]]
```

So how do we deal with the problem of set equality mentioned above, i.e. that `HashSet(1, 2)` and `HashSet(2, 1)` are not truly _equal_?
We just don't provide a definition of equality for `HashSet[Int]`.

```tut:fail
implicitly[Equal[HashSet[Int]]]
```

But that means we cannot have a `HashSet[HashSet[Int]]`! (Remember, for a `HashSet[A]`, we need an instance of `Equal[A]`, and we just showed we don't have an instance of `Equal[HashSet[Int]]`.)

```tut:fail
HashSet(HashSet(1, 2, 3, 4, 5))
```

But we can have a `HashSetoid[HashSet[Int], E]`, where `E` is _some_ equivalence on `HashSet[Int]`.

```tut
HashSet.of(HashSet(1, 2, 3, 4, 5))
```

`HashSet.of(elems)` is like `HashSet(elems)`, except it tries to infer the equivalence on the element type, instead of requiring it to be equality.

Notice the _equivalence tag_: `Setoid.ContentEquiv[Int, Equality.type]`. Its meaning is (again, for humans only) that two setoids are equivalent when they contain the same elements (here, of type `Int`), as compared by the given equivalence of elements (here, `Equality`).

The remaining question is: How does this work in the presence of _multiple useful equivalences?_

Let's define another equivalence on `Int` (in addition to the provided equality).

```tut:silent
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

```tut
HashSet.of(HashSet(1, 2, 3, 4, 5))
```

This still works, because `HashSet` requires an _equality_ on `Int`, and there is only one in the implicit scope (the newly defined equivalence `EqMod10` is _not_ equality). Let's try to create a "setoid of setoids of integers":

```tut:fail
HashSet.of(HashSet.of(1, 2, 3, 4, 5))
```

This fails, because there are now more equivalences on `Int` in scope. (There are now also multiple hash functions, which is what the error message actually says.) We need to be more specific:

```tut
HashSet.of(HashSet.of[Int, Mod10](1, 2, 3, 4, 5))
```

Finally, does it **prevent mixing up equivalences**? Let's see:

```tut
val s1 = HashSet(1,  2,  3,         11, 12, 13    )
val s2 = HashSet(    2,  3,  4,  5,         13, 14)
val t1 = HashSet.of[Int, Mod10](1,  2,  3,         11, 12, 13    )
val t2 = HashSet.of[Int, Mod10](    2,  3,  4,  5,         13, 14)
```

Combining compatible setoids:

```tut
s1 union s2
t1 union t2
```

Combining incompatible setoids:

```tut:fail
s1 union t2
t1 union s2
```


## Conclusion

We went one step further in the direction of type-safe equivalence in Scala compared to what is typically seen out in the wild today. There is nothing very sophisticated about this encoding. I think the major win is that we can design APIs so that the extra type parameter (the "equivalence tag") stays unnoticed by the user of the API as long as they only deal with _equalities_. As soon as the equivalence tag starts requesting our attention (via an ambiguous implicit or a type error), it is likely that the attention is justified.
