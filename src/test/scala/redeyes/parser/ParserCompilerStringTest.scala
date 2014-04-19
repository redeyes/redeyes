package redeyes.parser

import scalaz._
import scalaz.std.string._
import org.scalacheck.{Prop, Gen}

import CharParserCompiler._

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._

object CharParserCompilerTest extends Properties("CharParserCompiler") with ArbitraryParser {
  import CharParserCompiler._
  
  implicit class ValidationW[A, B](value: Validation[A, B]) {
    def isSuccess = !value.toEither.right.toOption.isEmpty
    
    def success = value.toEither.right.getOrElse(throw new RuntimeException("Failed to parse value: " + failure))
    
    def failure = value.toEither.left.get
    
    def isFailure = !value.toEither.left.toOption.isEmpty
  }
  
  property("<~<") = forAll { (p1: ParserWithExample, p2: ParserWithExample) =>
   val text = p1.example + p2.example
   
   val c1 = compile(p1.parser)
   val c2 = compile(p2.parser)
   
   val both = compile(p1.parser <~< p2.parser)
   
   c1.parseString(p1.example).success == both.parseString(text).success
 }

  property(">~>") = forAll { (p1: ParserWithExample, p2: ParserWithExample) =>
   val text = p1.example + p2.example
   
   val c1 = compile(p1.parser)
   val c2 = compile(p2.parser)
   
   val both = compile(p1.parser >~> p2.parser)
   
   both.parseString(text).success == c2.parseString(p2.example).success
 }
 
  property("<+>") = forAll { (p1: ParserWithExample, p2: ParserWithExample) =>
   val text = p1.example + p2.example
   
   val c1 = compile(p1.parser)
   val c2 = compile(p2.parser)
   
   val both = compile(p1.parser <+> p2.parser)
   
   val bothParse = both.parseString(text).success
   val indParse = c1.parseString(p1.example).success + c2.parseString(p2.example).success
   
   Prop(bothParse == indParse) :| "both parse = " + bothParse + ", indParse = " + indParse
 }

  property("<|>") = forAll { (p1: ParserWithExample, p2: ParserWithExample) =>
   val text = p1.example + p2.example
   
   val c1 = compile(p1.parser)
   val c2 = compile(p2.parser)
   
   val both = compile(p1.parser <|> p2.parser)
   
   both.parseString(p1.example).isSuccess && both.parseString(p2.example).isSuccess
 }
}