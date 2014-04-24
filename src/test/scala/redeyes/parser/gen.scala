package redeyes.parser

import scalaz._

import CharParserCompiler._

import org.scalacheck.{Prop, Gen, Properties}
import org.scalacheck.Prop._

object GeneratorTest extends Properties("Generator") with ArbitraryParser {
  import CharParserCompiler._
  
  property("generate only generates examples that can be parsed") = forAll { (p: ParserWithExample) =>
    compile(p.parser).parseString(p.example).fold(
      failure => Prop(false) :| "Parser " + p.parser + " failed to parse " + p.example + "; " + failure,
      success => Prop(true)
    )
  }
}