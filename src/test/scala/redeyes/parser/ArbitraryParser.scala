package redeyes.parser

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import scalaz.std.string._

trait ArbitraryParser {
  import CharParserCompiler._
 
  lazy val genParserString = for {
    ignoreCase <- Gen.oneOf(true, false)
    alphaStr <- Gen.alphaStr
  } yield if (ignoreCase) stringCI(alphaStr) else string(alphaStr)
  
  lazy val genParserIdentifier = Gen.identifier.map(string _)
  
  lazy val genParserNumStr = Gen.numStr.map(string _)
  
  lazy val genParserOr = for {
    p1 <- genParserT
    p2 <- genParserT
  } yield (p1 <+> p2) <|> p2 // The pain of <|> left bias
  
  lazy val genParserAnd = for {
    p1 <- genParserT
    p2 <- genParserT
  } yield p1 <&> p2
  
  lazy val genParserJoin = for {
    p1 <- genParserT
    p2 <- genParserT
  } yield p1 <+> p2
  
  lazy val genParserNT: Gen[Parser[String]] = Gen.oneOf(genParserOr, /*genParserAnd, */ genParserJoin)
  
  lazy val genParserT: Gen[Parser[String]] = Gen.oneOf(genParserString, genParserIdentifier, genParserNumStr)
  
  lazy val genParser: Gen[Parser[String]] = Gen.oneOf(genParserT, genParserNT)

  implicit lazy val arbParser: Arbitrary[Parser[String]] = Arbitrary(genParser)
  
  case class ParserWithExample(parser: Parser[String], example: String)
  
  lazy val genParserWithExample: Gen[ParserWithExample] = for {
    p <- genParser
    
    val ex = generate(p)
    
    if (!ex.isEmpty)
  } yield ParserWithExample(p, ex.head())
  
  implicit lazy val arbParserWithExample = Arbitrary(genParserWithExample)
}