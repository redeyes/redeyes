package redeyes.parser

trait Equiv[A, B] extends Function1[A, B] { self =>
  def unapply(value: B): A

  def inverse: Equiv[B, A] = new Equiv[B, A] {
    def apply(value: B): A = self.unapply(value)

    def unapply(value: A): B = self.apply(value)
  }

  def <> [C](that: Equiv[B, C]): Equiv[A, C] = new Equiv[A, C] {
    def apply(value: A): C = that.apply(self.apply(value))

    def unapply(value: C): A = self.unapply(that.unapply(value))
  }

  def mapfst[AA](f: Equiv[AA, A]): Equiv[AA, B] = f <> self

  def mapsnd[BB](f: Equiv[B, BB]): Equiv[A, BB] = self <> f

  def dimap[AA, BB](fst: Equiv[AA, A], snd: Equiv[B, BB]): Equiv[AA, BB] = fst <> self <> snd

  def left[C](c: C): Equiv[(A, C), B] = Equiv(to = t => apply(t._1), from = b => (unapply(b), c))

  def right[C](c: C): Equiv[(C, A), B] = Equiv(to = t => apply(t._2), from = b => (c, unapply(b)))
}
object Equiv {
  def apply[A, B](to: A => B, from: B => A) = new Equiv[A, B] {
    def apply(value: A): B = to(value)

    def unapply(value: B): A = from(value)
  }

  def const[A, B](a: A, b: B): Equiv[A, B] = Equiv(to = a => b, from = b => a)

  def id[A]: Equiv[A, A] = Equiv(identity[A] _, identity[A] _)

  def fail[A](msg: String): Equiv[A, A] = Equiv(Function.const(sys.error(msg)), Function.const(sys.error(msg)))

  // TODO: Shapeless Generic[A]???
}