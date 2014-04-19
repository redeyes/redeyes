package redeyes

import scalaz.Equal

package object parser {
  type <=> [A, B] = Equiv[A, B]

  implicit class EquivSyntax[A, B](to: A => B) {
    def <=> (from: B => A): A <=> B = Equiv(to = to, from = from)
  }
}