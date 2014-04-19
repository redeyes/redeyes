package redeyes.parser

import scalaz.std.string._

trait JsonGrammar {
  import CharParserCompiler._
  
  sealed trait Json
  case class JNumber(value: BigDecimal) extends Json
  case class JString(value: String) extends Json
  case class JBool(value: Boolean) extends Json
  case class JObj(values: Vector[(String, Json)]) extends Json
  case class JArr(values: Vector[Json]) extends Json
  case object JNull extends Json
  
  lazy val startQuote: Parser[String] = whitespace >~> string("\"")
  
  lazy val endQuote: Parser[String] = string("\"") <~< whitespace
  
  lazy val quotedChar: Parser[Char] = satisfy(c => c != '\\' && c != '"')
  
  lazy val quotedString: Parser[String] = whitespace >~> startQuote >~> quotedChar.many.string <~< endQuote <~< whitespace

  lazy val jnumber: Parser[Json] = (whitespace >~> number <~< whitespace).map(Equiv[BigDecimal, JNumber](JNumber.apply, _.value)).as[Json]
  
  lazy val jstring: Parser[Json] = quotedString.map(Equiv[String, JString](JString.apply, _.value)).as[Json]
  
  lazy val jbool: Parser[Json] = (token("true").const(true) <|> token("false").const(false)).map(
    Equiv[Boolean, JBool](JBool.apply, _.value)
  ).as[Json]
  
  lazy val jnull: Parser[Json] = token("null").const(JNull)
  
  lazy val field: Parser[(String, Json)] = quotedString <~< token(":") <~> json
  
  lazy val jobject: Parser[Json] = (token("{") >~> field.sepBy(token(",")) <~< token("}")).map(
    Equiv[Vector[(String, Json)], JObj](JObj.apply, _.values)
  ).as[Json]
  
  lazy val jarray: Parser[Json] = (token("[") >~> json.sepBy(token(",")) <~< token("]")).map(
    Equiv[Vector[Json], JArr](JArr.apply, _.values)
  ).as[Json]
  
  lazy val json: Parser[Json] = jnull <|> jbool <|> jnumber <|> jstring <|> jarray <|> jobject
}
object JsonGrammar extends JsonGrammar