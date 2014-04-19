package redeyes.parser

import redeyes.doc._

import scalaz._

import scalaz.std.tuple._

import scalaz.syntax.applicative._

/**
 * A compiler for character parsers using String input. Implemented using
 * mostly imperative code and minimizing allocations, although there's no 
 * static analyzer and optimizer, which will prove at least as significant 
 * in improving performance.
 *
 * Should probably switch away from string to using some generic text
 * representation.
 */
trait CharParserCompiler extends CharParserModule with ParserCompiler {
  case class Text(value: String, cursor: Int = 0)

  type Input = Text
  type ParseState[A] = (A, Input)

  def parseResult[A](state: ParseState[A]) = state._1

  implicit class CompiledParserOps[A](parser: CompiledParser[A]) {
    def parseString(s: String) = parser(Text(s)).map(parseResult _)
  }
  
  def compile[A](parser: Parser[A]): CompiledParser[A] = {
    // TODO: Identity map
    
    def compile0[A](parser: Need[Parser[A]]): Need[CompiledParser[A]] = parser.map { parser =>
      parser match {
        case End(CharSource) => Compiled.End
      
        case Fail => Compiled.Fail

        case GetOrFail(parser) => Compiled.GetOrFail(compile0(parser))
      
        case Filter(parser, f) => Compiled.Filter(compile0(parser), f)
      
        case Pure(value) => Compiled.Pure(value)

        case x @ Eq(left, right) => Compiled.Eq(compile0(left), compile0(right))(x.equal)
      
        case x : AtomParser[_, _] => // Scala inference fail
          x.atom match {
            case CharRange(min, max) => Compiled.CharRange(min, max).asInstanceOf[CompiledParser[A]]
          }
      
        case x @ Join(_, _) => Compiled.Join(x.flatten.map(_.map(compile _)))(x.semigroup)

        case x : Zip[_, _] => Compiled.Zip(compile0(x.left), compile0(x.right))
      
        case x @ Or(_, _) => Compiled.Or(x.flatten.map(_.map(compile _)))
      
        case Not(negated, _) => compile0(negated).value
      
        case LookAhead(parser) => Compiled.LookAhead(compile0(parser))

        case IfThenElse(pred, ifTrue, ifFalse) => Compiled.IfThenElse(compile0(pred), compile0(ifTrue), compile0(ifFalse))
      
        case Repeat(parser, min, max) => Compiled.Repeat(compile0(parser), min, max)
      
        case Map(parser, f) => Compiled.Map(compile0(parser), f)
      
        case Apply(f, value) => Compiled.Apply(compile0(f), compile0(value))
      
        case Described(parser, expected, _) => Compiled.Described(compile0(parser), expected)
      }
    }
    
    compile0(Need(parser)).value
  }
  
  def mkInput(value: String): Input = Text(value, 0)
  
  implicit def showInput: Show[Input] = new Show[Input] {
    override def show(value: Input): Cord = Cord(value.toString) // TODO
  }
  
  private object Compiled {
    @inline
    final def fail[A](docs: NonEmptyList[Doc], input: Input) = Validation.failure[ParseFail, A](ParseFail(docs, input))
    
    @inline
    final def refail[A](failure: ParseFail) = Validation.failure[ParseFail, A](failure)
    
    val UnexpectedEos = NonEmptyList(Doc.InlineText("Found unexpected end of string"))
    
    @inline
    final def reserveSpace[A](input: Input, spaces: Int)(f: => Validation[ParseFail, ParseState[A]]): Validation[ParseFail, ParseState[A]] = {
      if (input.value.length - input.cursor < spaces) fail(UnexpectedEos, input) else f
    }
    
    case object End extends CompiledParser[Unit] {
      val failureMessage = NonEmptyList(Doc.InlineText("Expected to find end of input"))
      
      def apply(input: Input) = {
        if (input.cursor == input.value.length) Validation.success(() -> input)
        else fail(failureMessage, input)
      }
    }
    
    case object Fail extends CompiledParser[Unit] {
      val failureMessage = NonEmptyList(Doc.InlineText("Parse forcibly terminated by grammar"))
      
      def apply(input: Input) = fail(failureMessage, input)
    }
    
    case class Pure[A](value: A) extends CompiledParser[A] {
      def apply(input: Input) = Validation.success(value -> input)
    }

    case class Eq[A: Equal](left: Need[CompiledParser[A]], right: Need[CompiledParser[A]]) extends CompiledParser[Boolean] {
      def apply(input: Input) = {
        left.value(input).fold(
          error => refail(error),
          success1 => {
            right.value(input).fold(
              error => refail(error),
              success2 => {
                val nextInput = if (success1._2.cursor > success2._2.cursor) success1._2 else success2._2

                Validation.success(Equal[A].equal(success1._1, success2._1) -> nextInput)
              }
            )
          }
        )
      }
    }
    
    // TODO: Use later
    case object AnyChar extends CompiledParser[Char] {
      def apply(input: Input) = reserveSpace(input, 1) {
        Validation.success(input.value.charAt(input.cursor) -> Text(input.value, input.cursor + 1))
      }
    }

    case class Filter[A](parser: Need[CompiledParser[A]], f: A => Boolean) extends CompiledParser[A] {
      def apply(input: Input) = {
        val result = parser.value(input)
        
        result.fold(
          error => result,
          success => {
            if (!f(success._1)) fail(NonEmptyList(Doc.InlineText("Parsed value fails to satisfy boolean predicate")), input)
            else Validation.success(success)
          }
        )
      }
    }

    case class GetOrFail[A](parser: Need[CompiledParser[Option[A]]]) extends CompiledParser[A] {
      def apply(input: Input) = {
        val result = parser.value(input)
        
        result.fold(
          refail _,
          {
            case (None, rest) => fail(NonEmptyList(Doc.InlineText("Parsed value fails to satisfy boolean predicate")), input)
            case (Some(a), rest) => Validation.success(a -> rest)
          }
        )
      }
    }
    
    // TODO: Use later
    case class Chr(value: Char) extends CompiledParser[Char] {
      val failureMessage = NonEmptyList(Doc.InlineText("Expected to find character '" + value + "'"))
      
      def apply(input: Input) = reserveSpace(input, 1) {
        if (input.value.charAt(input.cursor) == value) Validation.success(value -> Text(input.value, input.cursor + 1))
        else fail(failureMessage, input)
      }
    }
    
    case class CharRange(lower: Char, upper: Char) extends CompiledParser[Char] {
      val failureMessage = NonEmptyList(Doc.InlineText("Expected to find a character within the range '" + lower + "' - '" + upper + "'"))
      
      def apply(input: Input) = reserveSpace(input, 1) {
        val c = input.value.charAt(input.cursor)
        
        if (c >= lower && c <= upper) Validation.success(c -> Text(input.value, input.cursor + 1))
        else fail(failureMessage, input)
      }
    }
    
    // TODO: Use this (or use 2 case specialization)
    case class ChrIgnoreCase(value0: Char) extends CompiledParser[Char] {
      val value = Character.toUpperCase(value0)
      
      val failureMessage = NonEmptyList(Doc.InlineText("Expected to find character '" + value0 + "'"))
      
      def apply(input: Input) = reserveSpace(input, 1) {
        if (Character.toUpperCase(input.value.charAt(input.cursor)) == value) Validation.success(value -> Text(input.value, input.cursor + 1))
        else fail(failureMessage, input)
      }
    }
    
    case class Join[A: Semigroup](parsers: Vector[Need[CompiledParser[A]]]) extends CompiledParser[A] {
      var parsersLength = parsers.length
      
      def apply(input: Input) = {
        var acc: A = null.asInstanceOf[A]
        var curInput = input
        
        var parserIndex = 0
        
        var result: Validation[ParseFail, ParseState[A]] = null
        var matching = true

        while (parserIndex < parsersLength && matching) {
          val parser = parsers(parserIndex)
          
          result = parser.value(curInput)

          result.fold(
            error => { 
              matching = false
            },
            success => {
              acc = if (acc == null) success._1 else Semigroup[A].append(acc, success._1)

              curInput = success._2
            }
          )

          parserIndex = parserIndex + 1
        }

        if (!matching) result
        else Validation.success(acc -> curInput)
      }
    }

    case class Zip[A, B](left: Need[CompiledParser[A]], right: Need[CompiledParser[B]]) extends CompiledParser[(A, B)] {
      def apply(input: Input) = {
        left.value(input).fold(
          refail(_),
          {
            case (r1, rest) =>
              right.value(rest).fold(
                refail(_),
                {
                  case (r2, rest) =>
                    Validation.success((r1, r2) -> rest)
                }
              )
          }
        )
      }
    }

    case class IfThenElse[A](predicate: Need[CompiledParser[Boolean]], ifTrue: Need[CompiledParser[A]], ifFalse: Need[CompiledParser[A]]) extends CompiledParser[A] {
      def apply(input: Input) = {
        predicate.value(input).fold(
          refail _,
          success => {
            if (success._1) ifTrue.value(success._2)
            else ifFalse.value(success._2)
          }
        )
      }
    }
    
    case class Or[A](parsers: Vector[Need[CompiledParser[A]]]) extends CompiledParser[A] {
      def apply(input: Input) = {        
        var result = parsers(0).value(input)
        var i = 1
        
        while (result.isFailure && i < parsers.length) {
          result = parsers(i).value(input)
          
          i = i + 1
        }
        
        result
      }
    }
    
    case class LookAhead[A](parser: Need[CompiledParser[A]]) extends CompiledParser[A] {
      def apply(input: Input) = {
        val result = parser.value(input)
        
        result.fold(
          error   => result,
          success => Validation.success(success._1 -> input)
        )
      }
    }
    
    case class Repeat[A](parser: Need[CompiledParser[A]], min0: Option[Int], max0: Option[Int]) extends CompiledParser[Vector[A]] {
      val min = min0.getOrElse(0)
      val max = max0.getOrElse(Int.MaxValue)
      
      def apply(input: Input) = {
        var matched = 0
        var matching = true
        val builder = Vector.newBuilder[A]
        var result: Validation[ParseFail, ParseState[A]] = null
        
        var curInput = input
        
        while (matched < max && matching) {
          result = parser.value(curInput)
          
          result.fold(
            error => {
              matching = false
            },
            success => {
              matched = matched + 1
              
              builder += success._1
              curInput = success._2
            }
          )
        }
        
        if (matched >= min) Validation.success(builder.result -> curInput)
        else result.map(t => (Vector(t._1), t._2))
      }
    }
    
    case class Map[A, B](parser: Need[CompiledParser[A]], f: A <=> B) extends CompiledParser[B] {
      def apply(input: Input) = parser.value(input).map {
        case ((v, input)) => (f(v), input)
      }
    }
    
    case class Apply[A, B](f: Need[CompiledParser[A <=> B]], value: Need[CompiledParser[A]]) extends CompiledParser[B] {
      def apply(input: Input) = {
        f.value(input).fold(
          error   => refail(error),
          success => {
            val (f, input) = success
            
            value.value(input).fold(
              error   => refail(error),
              success => {
                val (a, input) = success
                
                Validation.success(f(a), input)
              }
            )
          }
        )
      }
    }
    
    case class Described[A](parser: Need[CompiledParser[A]], expected: Doc) extends CompiledParser[A] {
      val failureMessage = NonEmptyList(expected)
      
      def apply(input: Input) = {
        val result = parser.value(input)
        
        result.fold(
          error   => fail(failureMessage, input),
          success => result
        )
      }
    }
  }
}
object CharParserCompiler extends CharParserCompiler
