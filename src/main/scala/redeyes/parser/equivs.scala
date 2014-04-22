package redeyes.parser


/**
 * Equivalence functor. Or something.
 *
 * All instances must obey:
 *
 * map(fa)(Equiv.id) = fa
 * map(fa)(f <> g) = map(map(fa)(f))(g)
 * 
 * unmap(fb)(Equiv.id) = fb
 * unmap(fb)(f <> g) = unmap(unmap(fb)(f))(g)
 * 
 */
trait EquivFunctor[F[_]] {
  def map[A, B](fa: => F[A])(f: A <=> B): F[B]

  final def unmap[A, B](fb: => F[B])(f: A <=> B): F[A] = map(fb)(f.inverse)
}

/**
 * Equivalence zippable functor. I believe all the laws are "theorems for free".
 */
trait EquivZip[F[_]] extends EquivFunctor[F] {
  def zip2[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]

  def zip3[A, B, C](fa: => F[A], fb: => F[B], fc: => F[C]): F[(A, B, C)] = 
    map(zip2(zip2(fa, fb), fc))(Equiv(
      { case ((a, b), c) => (a, b, c) },
      { case (a, b, c)   => ((a, b), c)}
    ))

  def zip4[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D]): F[(A, B, C, D)] = 
    map(zip2(zip3(fa, fb, fc), fd))(Equiv(
      { case ((a, b, c), d) => (a, b, c, d) },
      { case (a, b, c, d)   => ((a, b, c), d)}
    ))

  def zip5[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E]): F[(A, B, C, D, E)] = 
    map(zip2(zip4(fa, fb, fc, fd), fe))(Equiv(
      { case ((a, b, c, d), e) => (a, b, c, d, e) },
      { case (a, b, c, d, e)   => ((a, b, c, d), e)}
    ))

  def zip6[A, B, C, D, E, G](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fg: => F[G]): F[(A, B, C, D, E, G)] = 
    map(zip2(zip5(fa, fb, fc, fd, fe), fg))(Equiv(
      { case ((a, b, c, d, e), g) => (a, b, c, d, e, g) },
      { case (a, b, c, d, e, g)   => ((a, b, c, d, e), g)}
    ))
}

/**
 * Equivalence Apply. 
 *
 * All instances must obey:
 *
 * ap1(fa)(ffab <> ffbc) = ap1(ap1(fa)(ffab))(ffbc)
 */
trait EquivApply[F[_]] extends EquivZip[F] {  
  def ap1[A,B](fa: => F[A])(f: => F[A <=> B]): F[B]  

  final def ap2[A,B,Z](fa: => F[A], fb: => F[B])(ff: F[(A,B) <=> Z]): F[Z] = 
    ap1(zip2(fa, fb))(ff)

  final def ap3[A,B,C,Z](fa: => F[A], fb: => F[B], fc: => F[C])(ff: F[(A,B,C) <=> Z]): F[Z] = 
    ap1(zip3(fa, fb, fc))(ff)

  final def ap4[A,B,C,D,Z](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(ff: F[(A,B,C,D) <=> Z]): F[Z] = 
    ap1(zip4(fa, fb, fc, fd))(ff)

  final def ap5[A,B,C,D,E,Z](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(ff: F[(A,B,C,D,E) <=> Z]): F[Z] = 
    ap1(zip5(fa, fb, fc, fd, fe))(ff)

  final def ap6[A,B,C,D,E,G,Z](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fg: => F[G])(ff: F[(A,B,C,D,E,G) <=> Z]): F[Z]=
    ap1(zip6(fa, fb, fc, fd, fe, fg))(ff)

  final def unap1[A,B](fb: => F[B])(ff: => F[A <=> B]): F[A] = 
    ap1(fb)(map[A <=> B, B <=> A](ff)(Equiv(_.inverse, _.inverse)))

  final def unap2[A,B,Z](fz: => F[Z])(ff: => F[(A, B) <=> Z]): F[(A, B)] = 
    ap1(fz)(map[(A, B) <=> Z, Z <=> (A, B)](ff)(Equiv(_.inverse, _.inverse)))

  final def unap3[A,B,C,Z](fz: => F[Z])(ff: => F[(A, B, C) <=> Z]): F[(A, B, C)] = 
    ap1(fz)(map[(A, B, C) <=> Z, Z <=> (A, B, C)](ff)(Equiv(_.inverse, _.inverse)))

  final def unap4[A,B,C,D,Z](fz: => F[Z])(ff: => F[(A, B, C, D) <=> Z]): F[(A, B, C, D)] = 
    ap1(fz)(map[(A, B, C, D) <=> Z, Z <=> (A, B, C, D)](ff)(Equiv(_.inverse, _.inverse)))

  final def unap5[A,B,C,D,E,Z](fz: => F[Z])(ff: => F[(A, B, C, D, E) <=> Z]): F[(A, B, C, D, E)] = 
    ap1(fz)(map[(A, B, C, D, E) <=> Z, Z <=> (A, B, C, D, E)](ff)(Equiv(_.inverse, _.inverse)))  

  final def unap6[A,B,C,D,E,G,Z](fz: => F[Z])(ff: => F[(A, B, C, D, E, G) <=> Z]): F[(A, B, C, D, E, G)] = 
    ap1(fz)(map[(A, B, C, D, E, G) <=> Z, Z <=> (A, B, C, D, E, G)](ff)(Equiv(_.inverse, _.inverse)))  
}

/**
 * Equivalence Applicative.
 *
 * TODO: Laws similar to Applicative.
 */
trait EquivApplicative[F[_]] extends EquivApply[F] {
  def point[A](a: => A): F[A]

  final def apply1[A, B](fa: => F[A])(f: => A <=> B): F[B] = ap1(fa)(point(f))

  final def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: => (A, B) <=> C): F[C] = ap2(fa, fb)(point(f))

  final def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: => (A, B, C) <=> D): F[D] = ap3(fa, fb, fc)(point(f))

  final def apply4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: => (A, B, C, D) <=> E): F[E] = ap4(fa, fb, fc, fd)(point(f))

  final def apply5[A, B, C, D, E, G](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: => (A, B, C, D, E) <=> G): F[G] = ap5(fa, fb, fc, fd, fe)(point(f))

  final def apply6[A, B, C, D, E, G, H](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fg: => F[G])(f: => (A, B, C, D, E, G) <=> H): F[H] = ap6(fa, fb, fc, fd, fe, fg)(point(f))

  final def lift1[A, B](f: => A <=> B) = (fa: F[A]) => apply1(fa)(f)

  final def lift2[A, B, C](f: => (A, B) <=> C) = (fa: F[A], fb: F[B]) => apply2(fa, fb)(f)

  final def lift3[A, B, C, D](f: => (A, B, C) <=> D) = (fa: F[A], fb: F[B], fc: F[C]) => apply3(fa, fb, fc)(f)

  final def lift4[A, B, C, D, E](f: => (A, B, C, D) <=> E) = (fa: F[A], fb: F[B], fc: F[C], fd: F[D]) => apply4(fa, fb, fc, fd)(f)

  final def lift5[A, B, C, D, E, G](f: => (A, B, C, D, E) <=> G) = (fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E]) => apply5(fa, fb, fc, fd, fe)(f)

  final def lift6[A, B, C, D, E, G, H](f: => (A, B, C, D, E, G) <=> H) = (fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G]) => apply6(fa, fb, fc, fd, fe, fg)(f)
}