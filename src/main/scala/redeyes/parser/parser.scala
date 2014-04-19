package redeyes.parser

import redeyes.doc._
import redeyes.util.Strings

import scalaz._
import scalaz.std.string._
import scalaz.std.vector._

import scalaz.syntax.monad._

import shapeless._
import shapeless.ops.hlist.{Comapped, ToList}

trait ParserModule { self =>
  /**
   * The abstract type representing the channels supported by this parser module.
   */
  protected type Channel

  protected implicit def ChannelEqual: Equal[Channel] // TODO: Move to functions

  /**
   * The abstract type representing an atom of information parsed from a channel.
   * For example, in a text parser module, the atom might be a character.
   */
  protected type Atom[C <: Channel, A]

  /**
   * Every parser module has to provide an atom algebra module.
   */
  protected trait AtomAlgebra {
    /**
     * An atom representing the smallest unit of information that can be produced
     * from the specified channel. This should not be "empty" but should actually
     * represent consumption of some information.
     */
    def anything[C <: Channel](channel: C): Atom[C, _]

    /**
     * Produces a parser which is the negation of the specified atom.
     */
    def negate[C <: Channel, A](atom: Atom[C, A]): Parser[A]

    /**
     * Produces a parser which is the intersection of two atoms.
     */
    def intersect[C <: Channel, A: Equal](a1: Atom[C, A], a2: Atom[C, A]): Parser[A]
  }

  protected def AtomAlgebra: AtomAlgebra

  def generate[A](parser: Parser[A]): EphemeralStream[A]

  def generate1[A](parser: Parser[A]): A = generate(parser).head()

  /**
   * The core parser abstraction, with a range of combinators.
   */
  sealed trait Parser[A] {
    import Parser.ParserIsoApplicative._

    final def |> [B] (f: => A <=> B): Parser[B] = this <**> Pure(f)
    
    final def <**> [B] (that: => Parser[A <=> B]): Parser[B] = Apply[A, B](Need(that), Need(this))
    
    final def <~< [B] (that: => Parser[B]): Parser[A] = lift2[A, B, A](Equiv.id[A].left(generate1(that)))(this, that)
    
    final def >~> [B] (that: => Parser[B]): Parser[B] = lift2[A, B, B](Equiv.id[B].right(generate1(this)))(this, that)
    
    final def <|> (that: => Parser[A]): Parser[A] = Or(Need(this), Need(that))
    
    final def <&> (that: => Parser[A])(implicit equal: Equal[A]): Parser[A] = Intersect(Need(this), Need(that))
    
    final def <~> [B](that: => Parser[B]): Parser[(A, B)] = Zip(Need(this), Need(that))

    final def <+> (that: => Parser[A])(implicit m: Semigroup[A]): Parser[A] = Join(Need(this), Need(that))

    final def const[B](k: B): Parser[B] = map(Equiv(to = a => k, from = b => generate1(this)))

    final def maybeAs[B](implicit ev: A <:< B, A: scala.reflect.ClassTag[A]): Parser[Option[B]] = maybe.map(Equiv[Option[A], Option[B]](
      to   = oa => oa.map(ev), 
      from = ob => ob match {
        case None => None
        case Some(b) => if (A.runtimeClass.isAssignableFrom(b.getClass)) Some(b.asInstanceOf[A]) else None
      }
    ))

    final def as[B](implicit ev: A <:< B, A: scala.reflect.ClassTag[A]): Parser[B] = maybeAs[B].getOrFail
    
    final def sepBy[B](sep: => Parser[B]): Parser[Vector[A]] = (this.one <+> (sep >~> this).many) <|> 
      this.maybe.map(Equiv[Option[A], Vector[A]](_.toVector, _.headOption))
    
    final def sepBy1[B](sep: => Parser[B]): Parser[Vector[A]] = this.one <+> (sep >~> this).many
    
    final def maybe : Parser[Option[A]] = atMost(1).map(Equiv[Vector[A], Option[A]](_.headOption, _.toVector))
    
    final def many : Parser[Vector[A]] = Repeat(Need(this), None, None)
    
    final def some : Parser[Vector[A]] = Repeat(Need(this), Some(1), None)
    
    final def one: Parser[Vector[A]] = exactly(1)
    
    final def exactly(count: Int) : Parser[Vector[A]] = Repeat(Need(this), Some(count), Some(count))
    
    final def atLeast(min: Int) : Parser[Vector[A]] = Repeat(Need(this), Some(min), None)
    
    final def atMost(max: Int) : Parser[Vector[A]] = Repeat(Need(this), None, Some(max))
    
    final def between(min: Int, max: Int) : Parser[Vector[A]] = Repeat(Need(this), Some(min), Some(max))
    
    final def filter(f: A => Boolean): Parser[A] = Filter(Need(this), f)
    
    final def ^! [B: Documented, C: Documented](expected: B, unexpected: C): Parser[A] = 
      Described(Need(this), Documented[B].document(expected), Documented[C].document(unexpected))
    
    final def ^! [B: Documented](expected: B): Parser[A] = Described(Need(this), Documented[B].document(expected), Doc.Empty)
    
    final def unary_! : Parser[A] = Not(Need(negate), Need(this))
    
    final def map[B](f: A <=> B): Parser[B] = Map(Need(this), f)

    final def show(implicit s: Show[A]) = Cord(toString)

    final def === (that: => Parser[A])(implicit equal: Equal[A]): Parser[Boolean] = Eq(Need(this), Need(that))
    
    protected def negate: Parser[A]
  }

  implicit class ParserEquivSyntax[A, B](value: Parser[A <=> B]) {  
    @inline final def <*> (that: => Parser[A]): Parser[B] = that <**> value
  }

  implicit class ParserOptionSyntax[A](value: Parser[Option[A]]) {
    def getOrFail: Parser[A] = self.getOrFail(value)
  }

  trait ParserInstances {
    implicit def ParserShow[A: Show]: Show[Parser[A]] = Show.show(_.show)
    
    def ParserAndSemigroup[A: Semigroup: Equal]: Semigroup[Parser[A]] = new Semigroup[Parser[A]] {
      def append(a1: Parser[A], a2: => Parser[A]) = a1 <&> a2
    }
    
    def ParserOrSemigroup[A]: Semigroup[Parser[A]] = new Semigroup[Parser[A]] {
      def append(a1: Parser[A], a2: => Parser[A]) = a1 <|> a2
    }  

    implicit val ParserIsoApplicative = new IsoApplicative[Parser] {
      def point[A](a: => A): Parser[A] = Pure(a)

      def ap1[A, B](fa: => Parser[A])(f: => Parser[A <=> B]): Parser[B] = Apply(Need(f), Need(fa))

      def map[A, B](fa: => Parser[A])(f: A <=> B): Parser[B] = Map(Need(fa), f)

      def zip[A, B](fa: => Parser[A], fb: => Parser[B]): Parser[(A, B)] = zip2(fa, fb)
    }
  }

  object Parser extends ParserInstances

  protected def end(channel: Channel): Parser[Unit] = End(channel)

  protected def atom[C <: Channel, A](channel: C, atom: Atom[C, A]): Parser[A] = AtomParser(channel, atom)

  def zip2[A, B](pa: => Parser[A], pb: => Parser[B]): Parser[(A, B)] = Zip(Need(pa), Need(pb))
  
  def constant[A](value: A): Parser[A] = Pure(value)
  
  def lookAhead[A](parser: => Parser[A]): Parser[A] = LookAhead(Need(parser))
  
  def fail[A, B: Documented](expected: B): Parser[A] = (Fail.as[A] ^! Documented[B].document(expected))
  
  def fail[A, B: Documented, C: Documented](expected: B, unexpected: C): Parser[A] = 
    (Fail.as[A] ^! (Documented[B].document(expected), Documented[C].document(unexpected)))

  def ifThenElse[A](predicate: => Parser[Boolean])(ifTrue: => Parser[A], ifFalse: => Parser[A]) = {
    IfThenElse(Need(predicate), Need(ifTrue), Need(ifFalse))
  }

  def getOrFail[A](p: Parser[Option[A]]): Parser[A] = GetOrFail(Need(p))

  def check[A, B](condition: => Parser[A])(then: => Parser[B], orElse: => Parser[B]): Parser[B] = (lookAhead(condition) >~> then) <|> orElse
  
  def choice[A](p1: Parser[A], ps: Parser[A]*): Parser[A] = ps.foldLeft(p1)(_ <|> _)
  
  def manyTill[A, B](parser: => Parser[A], limit: => Parser[B]): Parser[Vector[A]] = (lookAhead(!limit) >~> parser).many

  def switch[A](catchAll: Parser[A])(cases: (Parser[Boolean], Parser[A])*): Parser[A] = {
    if (cases.length == 0) catchAll
    else {
      val (predicate, ifTrue) = cases.head

      ifThenElse(predicate)(ifTrue = ifTrue, ifFalse = switch(catchAll)(cases.tail: _*))
    }
  }

  protected final case class AtomParser[C <: Channel, A](channel: C, atom: Atom[C, A]) extends Parser[A] {
    def negate : Parser[A] = AtomAlgebra.negate(atom)

    override def toString = "AtomParser(" + atom + ")"
  }

  /**
   * A parser that expects the end of input for the specified channel, and will 
   * fail if there is more input.
   */
  protected final case class End(channel: Channel) extends Parser[Unit] {
    def negate : Parser[Unit] = LookAhead(Need(AtomParser(channel, AtomAlgebra.anything(channel)))).const(Unit)
    
    override def toString = "End(" + channel + ")"
  }  

  /**
   * A parser that consumes no input and always fails. Because this parser consumes
   * no input, it is not necessary for it to be parameterized by the channel.
   */
  protected final case object Fail extends Parser[Unit] {
    def negate : Parser[Unit] = Pure(Unit)

    def as[A]: Parser[A] = this.map(Equiv(to = unit => sys.error("impossible"), from = a => Unit)) // ???
    
    override def toString = "Fail"
  }

  /**
   * A parser that filters for elements that pass a specified predicate.
   */
  protected final case class Filter[A](parser: Need[Parser[A]], f: A => Boolean) extends Parser[A] { self =>
    def negate: Parser[A] = new Filter(parser, a => !f(a))

    override def toString = "Filter(" + parser.value + ", " + f + ")"
  }  

  protected final case class GetOrFail[A](parser: Need[Parser[Option[A]]]) extends Parser[A] { self =>
    def negate: Parser[A] = GetOrFail[A](parser.map(p => !p))

    override def toString = "GetOrFail(" + parser.value + ")"
  }  

  /**
   * A parser that consumes no input and always succeeds with the specified value.
   */
  protected final case class Pure[A](value: A) extends Parser[A] { 
    def negate : Parser[A] = this

    override def toString = "Pure(" + value + ")"
  }  

  /**
   * A parser that parsers a number of other parsers in some unspecified order.
   */
  /* protected case class AllOf[L <: HList, O <: HList](parsers: L)(implicit val cm: Comapped.Aux[L, Parser, O], toList: ToList[L, Parser[_]]) extends Parser[O] {
    def negate : Parser[O] = {
      val parsers1: List[Parser[_]] = parsers.toList

      
      ???
    }

    override def toString = "AllOf(" + parsers + ")"
  } */

  /**
   * A parser that consumes no input but produces the output of another parser.
   */
  protected final case class LookAhead[A](parser: Need[Parser[A]]) extends Parser[A] {
    def negate : Parser[A] = LookAhead(Need(!parser.value))

    override def toString = "LookAhead(" + parser.value + ")"
  }  

  /**
   * A parser that determines the equivalence of two other parsers.
   *
   * Note: This parser is redundant and is included only for performance reasons.
   */
  protected final case class Eq[A: Equal](left: Need[Parser[A]], right: Need[Parser[A]]) extends Parser[Boolean] {
    def equal: Equal[A] = Equal[A]

    def negate : Parser[Boolean] = Eq(left, right)(new Equal[A] {
      def equal(a1: A, a2: A): Boolean = !Equal[A].equal(a1, a2)
    })

    override def toString = "Eq(" + left.value + ", " + right.value + ")"
  }

  /**
   * A parser that joins together the output of two other parsers (of the same type)
   * using a provided semigroup.
   *
   * Note: This parser is redundant and is included only for performance reasons.
   */
  protected final case class Join[A: Semigroup](left: Need[Parser[A]], right: Need[Parser[A]]) extends Parser[A] {
    def semigroup: Semigroup[A] = Semigroup[A]
    
    def flatten: Vector[Need[Parser[A]]] = {
      def flatten0(v: Parser[A]): Need[Vector[Need[Parser[A]]]] = v match {
        case x : Join[A] => for {
          left <- x.left
          right <- x.right          
          leftFlattened <- flatten0(left)
          rightFlattened <- flatten0(right)
        } yield leftFlattened ++ rightFlattened
        
        case _ => Need(Vector(Need(v)))
      }
      
      flatten0(this).value
    }
    
    def negate : Parser[A] = {
      val negatedLeft = left.map(p => !p)
      val negatedRight = right.map(p => !p)

      Join(negatedLeft, negatedRight) <|>
      Join(left, negatedRight) <|>
      Join(negatedLeft, right)
    }

    override def toString = "Join(" + left.value + ", " + right.value + ")"
  }

  protected final case class Intersect[A: Equal](left: Need[Parser[A]], right: Need[Parser[A]]) extends Parser[A] {
    def equal: Equal[A] = Equal[A]
    
    def flatten: Vector[Need[Parser[A]]] = {
      def flatten0(v: Parser[A]): Need[Vector[Need[Parser[A]]]] = v match {
        case x : Intersect[A] => for {
          left <- x.left
          right <- x.right          
          leftFlattened <- flatten0(left)
          rightFlattened <- flatten0(right)
        } yield leftFlattened ++ rightFlattened
        
        case _ => Need(Vector(Need(v)))
      }
      
      flatten0(this).value
    }
    
    def negate : Parser[A] = {
      val negatedLeft = left.map(p => !p)
      val negatedRight = right.map(p => !p)

      Intersect(negatedLeft, negatedRight) <|>
      Intersect(left, negatedRight) <|>
      Intersect(negatedLeft, right)
    }

    override def toString = "Join(" + left.value + ", " + right.value + ")"
  }

  protected final case class Zip[A, B](left: Need[Parser[A]], right: Need[Parser[B]]) extends Parser[(A, B)] {
    def negate: Parser[(A, B)] = {
      val negatedLeft = left.map(p => !p)
      val negatedRight = right.map(p => !p)

      Zip(negatedLeft, negatedRight) <|>
      Zip(left, negatedRight) <|>
      Zip(negatedLeft, right)
    }

    override def toString = "Zip(" + left.value + ", " + right.value + ")"
  }

  /**
   * Parsers a boolean, then based on the value of that boolean, parses with one of two
   * provided parsers. This parser provides the ability to do simple context-sensitive
   * parsing. It is the only way of doing such parsing within this framework.
   */
  protected final case class IfThenElse[A](predicate: Need[Parser[Boolean]], ifTrue: Need[Parser[A]], ifFalse: Need[Parser[A]]) extends Parser[A] {
    def negate : Parser[A] = {
      // TODO: Include other possibilities
      IfThenElse(predicate, ifTrue.map(v => !v), ifFalse.map(v => !v))
    }

    override def toString = "IfThenElse(" + predicate.value + ", " + ifTrue.value + "," + ifFalse.value + ")"
  }

  /**
   * A parser that parsers one of two alternatives. This parser is left-biased.
   */
  protected final case class Or[A](left: Need[Parser[A]], right: Need[Parser[A]]) extends Parser[A] {
    def flatten: Vector[Need[Parser[A]]] = {
      def flatten0(v: Parser[A]): Need[Vector[Need[Parser[A]]]] = v match {
        case x : Or[A] => for {
          left <- x.left
          leftMapped <- flatten0(left)
          right <- x.right
          rightMapped <- flatten0(right)
        } yield leftMapped ++ rightMapped
        
        case _ => Need(Vector(Need(v)))
      }
      
      flatten0(this).value
    }
    
    def negate : Parser[A] = ??? // TODO: !left.value <&> !right.value

    override def toString = "Or(" + left.value + ", " + right.value + ")"
  }

  /**
   * A parser that is the negation of some other parser. 
   */
  protected final case class Not[A](negated: Need[Parser[A]], original: Need[Parser[A]]) extends Parser[A] {
    def negate : Parser[A] = original.value

    override def toString = "Not(" + negated.value + ", " + original.value + ")"
  }  

  /**
   * A parser that repeats another parser within specified ranges.
   */
  protected final case class Repeat[A](parser: Need[Parser[A]], min: Option[Int], max: Option[Int]) extends Parser[Vector[A]] {
    def negate : Parser[Vector[A]] = ???

    override def toString = "Repeat(" + parser.value + ", " + min + ", " + max + ")"
  }

  /**
   * A parser that maps the value of one parser into another value using a provided function.
   */
  protected final case class Map[A, B](parser: Need[Parser[A]], f: A <=> B) extends Parser[B] {
    def negate : Parser[B] = Map(parser.map(v => !v), f)

    override def toString = "Map(" + parser.value + ", " + f + ")"
  }

  /**
   * A parser that applies a parser of a function to a parser of a value.
   */
  protected final case class Apply[A, B](f: Need[Parser[A <=> B]], value: Need[Parser[A]]) extends Parser[B] {
    def negate : Parser[B] = {
      // Apply needs to parse f and then value to succeed. So we can produce
      // elements not matched by this parser by negating f, value, or both.
      Apply(f.map(p => !p), value.map(p => !p)) <|>
      Apply(f, value.map(p => !p)) <|>
      Apply(f.map(p => !p), value)
    }

    override def toString = "Apply(" + f + ", " + value.value + ")"
  }

  /**
   * A parser that describes failure cases for another parser.
   */
  protected final case class Described[A](parser: Need[Parser[A]], expected: Doc, unexpected: Doc) extends Parser[A] {
    def negate : Parser[A] = Described(parser.map(p => !p), unexpected, expected)

    override def toString = "Described(" + parser.value + ")"
  }
}