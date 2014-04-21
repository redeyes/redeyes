package redeyes.parser

/**
 * Defines an equivalence between types A and B.
 * 
 * Let ~_A be an equivalence relation on A, and ~_B be an equivalence relation on B.
 * Then Equiv[A, B](to, from) must satisfy the following laws:
 *
 * 1. forall a in A. (from compose to)(a) ~_A a
 * 2. forall b in B. (to compose from)(b) ~_B b
 *
 * Another way to define this is as a bijection between the equivalence classes of A,
 * and the equivalence classes of B, together with two choice functions c_A and c_B,
 * which given an equivalence class, will deterministically choose an arbitrary element
 * of those classes ("canonical element").
 */ 
final case class Equiv[A, B](to: A => B, from: B => A) extends Function1[A, B] { self =>
  def apply(a: A): B = to(a)

  def unapply(b: B): A = from(b)

  def inverse: Equiv[B, A] = Equiv(from, to)

  def <> [C](that: Equiv[B, C]): Equiv[A, C] = Equiv(
    to    = that.to compose to,
    from  = from compose that.from
  )

  def left[C](c: C): Equiv[(A, C), B] = Equiv(to = t => apply(t._1), from = b => (unapply(b), c))

  def right[C](c: C): Equiv[(C, A), B] = Equiv(to = t => apply(t._2), from = b => (c, unapply(b)))
}
object Equiv {
  def const[A, B](a: A, b: B): Equiv[A, B] = Equiv(to = a => b, from = b => a)

  def id[A]: Equiv[A, A] = Equiv(identity[A] _, identity[A] _)

  def fail[A](msg: String): Equiv[A, A] = Equiv(Function.const(sys.error(msg)), Function.const(sys.error(msg)))

  // TODO: Shapeless Generic[A]???
}