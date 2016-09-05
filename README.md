# hasheq
This library provides typeclass-based implementation of `HashSet` and `HashMap` data structures, both mutable and immutable, for Scala.

This library is based on the source code of the Scala standard library and is distributed under the same license. Thanks to all the original contributors!

## Try it out
We don't have a release yet. To try out, include the following in your `build.sbt`:
```sbt
lazy val root = (project in file(".")).
  dependsOn(RootProject(uri("https://github.com/TomasMikula/hasheq.git#v0.1")))
```

## `HashEq` typeclass
To work with `HashSet[A]`/`HashMap[A]` from this library, you will need to define equality and hash-code for type `A` via an (implicit) instance of [`HashEq[A]`](https://github.com/TomasMikula/hasheq/blob/master/src/main/scala/hasheq/HashEq.scala) typeclass.

## Abstracting over set/map implementation
You may notice that the data structures in this library do not implement some common interface for sets/maps, such as `scala.collection.Set`/`scala.collection.Map`. To write generic code that doesn't care about the underlying implementation of set/map (such as `HashSet`, `ListSet`, `TreeSet`, ...), you can
 - use a type parameter `S[_]` and an (implicit) instance of [`SetRepr[S, A]`](https://github.com/TomasMikula/hasheq/blob/master/src/main/scala/hasheq/immutable/SetRepr.scala) to abstract over set implementation;
 - use a type parameter `M[_, _]` and an (implicit) instance of [`MapRepr[M, K]`](https://github.com/TomasMikula/hasheq/blob/master/src/main/scala/hasheq/immutable/MapRepr.scala) to abstract over map implementation.

## Status
This is work in progress. Both `HashSet` and `HashMap`, mutable and immutable, have been ported from the Scala standard library (`equals`/`hashCode`-based) to typeclass-style. However, currently only `immutable.HashSet` is tested for correctness. Before using the others in production, you might want to contribute tests for them.
