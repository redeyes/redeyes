package redeyes.parser

import redeyes.doc._

import scalaz._

/**
 * A module that adds the ability to compile parsers down to executable form
 * for some input type.
 */
trait ParserCompiler { self: ParserModule =>
  type Input
  
  case class ParseFail(errors: NonEmptyList[Doc], input: Input)
  type ParseState[A]
  
  trait CompiledParser[A] extends (Input => Validation[ParseFail, ParseState[A]])

  def compile[A](parser: Parser[A]): CompiledParser[A]

  def parseResult[A](value: ParseState[A]): A
}