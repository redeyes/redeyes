package redeyes.parser

/**
 * Defines an equivalence between types A and B.
 * 
 * Let ~_A be an equivalence relation on A, and ~_B be an equivalence relation
 * on B.
 *
 * Let [a] denote the equivalence class of a in A ({a' | a ~_A a'}), and [b] 
 * denote the equivalence class of b in B ({b' | b ~_B b'}).
 *
 * Let `f: {[a] | a in A} -> {[b] | b in B]`, 'g: {[b] | b in B} -> {[a] | a in A}' 
 * define a bijection between the equivalence classes of A and B.
 *
 * Let `c_A: [a] -> a`  and `c_B: [b] -> b` define two choice functions, which 
 * will map every equivalence class to a choice of one element in that class 
 * ("canonical element").
 *
 * Equiv[A, B](to, from) is then defined as follows:
 *
 * to(a)   = c_B(f([a]))
 * from(b) = c_A(g([b]))
 *
 * An equivalence between types A and B is *not* an isomorphism, because it 
 * does not satisfy the identity laws. It is, however, more general than an
 * isomorphism and is closely related to equivalence categories in category 
 * theory.
 */ 
final case class Equiv[A, B](to: A => B, from: B => A) extends Function1[A, B] { self =>
  def apply(a: A): B = to(a)

  def unapply(b: B): A = from(b)

  def inverse: Equiv[B, A] = Equiv(from, to)

  def <> [C](that: Equiv[B, C]): Equiv[A, C] = Equiv(
    to    = that.to compose to,
    from  = from compose that.from
  )

  def leftfst[C](c: C): Equiv[(A, C), B] = Equiv(to = t => apply(t._1), from = b => (unapply(b), c))

  def leftsnd[C](c: C): Equiv[(C, A), B] = Equiv(to = t => apply(t._2), from = b => (c, unapply(b)))

  def rightfst[C](c: C): Equiv[A, (B, C)] = Equiv(to = a => (apply(a), c), from = bc => unapply(bc._1))

  def rightsnd[C](c: C): Equiv[A, (C, B)] = Equiv(to = a => (c, apply(a)), from = cb => unapply(cb._2))
}
object Equiv {
  def const[A, B](a: A, b: B): Equiv[A, B] = Equiv(to = a => b, from = b => a)

  def id[A]: Equiv[A, A] = Equiv(identity[A] _, identity[A] _)

  def undefined[A, B](msg: String): Equiv[A, B] = Equiv(Function.const(sys.error(msg)), Function.const(sys.error(msg)))

  // TODO: Shapeless Generic[A]???
}