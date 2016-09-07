package object hasheq {
  type HashEqual[A] = Hash[A, Equal[A]]
}
