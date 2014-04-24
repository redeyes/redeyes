package redeyes.parser

import redeyes.doc._
import redeyes.util.Strings

import scalaz.{NonEmptyList, Traverse1, Equal, EphemeralStream}
import scalaz.NonEmptyList._

import scalaz.std.vector._
import scalaz.std.string._
import scalaz.std.anyVal._

trait CharParserModule extends ParserModule with Generator {
  protected sealed trait CharSource
  protected type Channel = CharSource
  protected object CharSource extends Channel

  protected implicit val ChannelEqual = new Equal[Channel] {
    def equal(v1: Channel, v2: Channel) = true
  }

  protected sealed trait CharAtoms[C <: Channel, A]
  protected type Atom[C <: Channel, A] = CharAtoms[C, A]

  protected case class CharRange(min: Char, max: Char) extends Atom[CharSource, Char]

  protected val AtomAlgebra = new AtomAlgebra {
    private def toParser[A]: Atom[Channel, A] => Parser[A] = a => atom(CharSource, a)

    def anything[C <: Channel](channel: C) = CharRange(Char.MinValue, Char.MaxValue).asInstanceOf[Atom[C, _]]

    def negate[C <: Channel, A](atom: Atom[C, A]) : Parser[A] = atom match {
      case CharRange(min, max) =>
        val lower = if (min == Char.MinValue) None else Some(CharRange(Char.MinValue, (min - 1).toChar))
        val upper = if (max == Char.MaxValue) None else Some(CharRange((max + 1).toChar, Char.MaxValue))
        
        (lower, upper) match {
          case (None, None) => fail(Doc.InlineText("Expected end of input"), Doc.InlineText("Unexpected end of input"))
          case (lower, upper) => 
            val both = lower.map(toParser).toList ++ upper.map(toParser).toList
            choice(both.head, both.tail: _*)
        }
    }

    def intersect[C <: Channel, A: Equal](a1: Atom[C, A], a2: Atom[C, A]): Parser[A] = (a1, a2) match {
      case (CharRange(min1, max1), CharRange(min2, max2)) =>
        val min = if (min1 > min2) min1 else min2
        val max = if (max1 < max2) max1 else max2

        if (min > max) fail(Doc.InlineText("Expected end of input"), Doc.InlineText("Unexpected end of input"))
        else toParser(CharRange(min, max))
    }
  }

  protected def generateAtom[C <: Channel, A](channel: C, atom: Atom[C, A]): EphemeralStream[A] = {
    atom match {
      case CharRange(min, max) => 
        val next: Int => (Char, Int) = if (max == min) i => (min, i) else i => (i.toChar, ((i - min + 1) % (max - min)) + min)
        
        EphemeralStream.unfold(min.toInt)(cur => Some(next(cur)))
    }
  }

  //////////////////////////////

  def charRange(min: Char, max: Char): Parser[Char] = 
    atom(CharSource, CharRange(min, max)) ^! 
    (Doc.InlineText("Expected character between '" + min + "' and '" + max + "'"), 
     Doc.InlineText("Unexpected character between '" + min + "' and '" + max + "'"))
  
  implicit class ParserStringW(value: Parser[String]) {
    def chars = value.map(Equiv[String, Vector[Char]](_.toVector, Strings.vectorToString _))
  }

  implicit class ParserVectorCharW(value: Parser[Vector[Char]]) {
    def string = value.map[String](Equiv[Vector[Char], String](Strings.vectorToString _, _.toVector))
  }

  implicit val CharEqual = new Equal[Char] {
    def equal(c1: Char, c2: Char) = c1 == c2
  }

  val charEnd = end(CharSource)
  
  def string(value: String): Parser[String] = mkString(value, char _)
  
  def stringCI(value: String): Parser[String] = mkString(value, charCI _)

  val anyChar = charRange(Char.MinValue, Char.MaxValue) ^! 
    (Doc.InlineText("Expected any character"), Doc.InlineText("Unexpected character"))
  
  def satisfy(f: rangeDsl.CharParam.type => rangeDsl.CharExp): Parser[Char] = {
    f(rangeDsl.CharParam).parser(charRange _)
  }

  def satisfyCI(f: rangeDsl.CharParam.type => rangeDsl.CharExp): Parser[Char] = {
    f(rangeDsl.CharParam).parser(charRangeCI _)
  }
  
  def char(value: Char): Parser[Char] = charRange(value, value) ^! 
    (Doc.InlineText("Expected character '" + value + "'"), Doc.InlineText("Unexpected character '" + value + "'"))  
  
  def charCI(value: Char): Parser[Char] = ({
    val lc = Character.toLowerCase(value)
    val uc = Character.toUpperCase(value)
    
    char(lc) <|> char(uc)
  }) ^! (Doc.InlineText("Expected character '" + value + "'"), Doc.InlineText("Unexpected character '" + value + "'"))
  
  def charRangeCI(min: Char, max: Char): Parser[Char] = {
    (charRange(Character.toLowerCase(min), Character.toLowerCase(max)) <|> 
    charRange(Character.toLowerCase(min), Character.toLowerCase(max))) ^! 
      (Doc.InlineText("Expected character between '" + min + "' and '" + max + "'"), 
       Doc.InlineText("Unexpected character between '" + min + "' and '" + max + "'"))
  }
  
  val alphaLower: Parser[Char] = charRange('a', 'z')
  
  val alphaUpper: Parser[Char] = charRange('A', 'Z')
  
  val alpha: Parser[Char] = (alphaLower <|> alphaUpper) ^!
    (Doc.InlineText("Expected alphabetical character"), Doc.InlineText("Unexpected alphabetical character"))
  
  val digitChar: Parser[Char] = charRange('0', '9') ^!
    (Doc.InlineText("Expected digit character"), Doc.InlineText("Unexpected digit character"))
  
  val integer: Parser[BigInt] = digitChar.atLeast(1).string.map[BigInt](Equiv[String, BigInt](BigInt.apply _, _.toString)) ^!
    (Doc.InlineText("Expected integer"), Doc.InlineText("Unexpected integer"))
  
  val decimal: Parser[BigDecimal] = (digitChar.some <+> char('.').one <+> digitChar.many).string.map(Equiv[String, BigDecimal](BigDecimal.apply _, _.toString)) ^!
    (Doc.InlineText("Expected decimal"), Doc.InlineText("Unexpected decimal"))

  val number: Parser[BigDecimal] = (decimal <|> integer.map(Equiv[BigInt, BigDecimal](BigDecimal.apply _, _.toBigInt))) ^!
    (Doc.InlineText("Expected number"), Doc.InlineText("Unexpected number"))
  
  val identifier: Parser[String] = ((alpha <|> char('_')).one <+> ((alpha <|> char('_') <|> digitChar).many)).string ^!
    (Doc.InlineText("Expected identifier"), Doc.InlineText("Unexpected identifier"))
  
  val whitespace: Parser[String] = (char(' ') <|> char('\t') <|> char('\n') <|> char('\r')).many.string ^!
    (Doc.InlineText("Expected whitespace"), Doc.InlineText("Unexpected whitespace"))
  
  val line: Parser[String] = manyTill(anyChar, satisfy(c => c != '\n' && c != '\r')).string
  
  val newline: Parser[String] = (char('\r') <|> char('\n')).some.string ^!
    (Doc.InlineText("Expected newline"), Doc.InlineText("Unexpected newline"))
  
  val anyString: Parser[String] = anyChar.many.string
  
  def token(value: String): Parser[String] = (whitespace >~> string(value)) ^! 
    (Doc.InlineText("Expected token " + value), Doc.InlineText("Unexpected token '" + value + "'"))
  
  def tokenCI(value: String): Parser[String] = (whitespace >~> stringCI(value)) ^! 
    (Doc.InlineText("Expected token " + value), Doc.InlineText("Unexpected token '" + value + "'"))
  
  def identifier(reserved: Parser[String]): Parser[String] = (whitespace >~> lookAhead(!reserved) >~> identifier) ^!
    (Doc.InlineText("Expected non-reserved identifier"), Doc.InlineText("Unexpected non-reserved identifier"))
  
  def identifier(reserved: NonEmptyList[Parser[String]]): Parser[String] = identifier(choice(reserved.head, reserved.tail: _*))

  private def mkString(value: String, f: Char => Parser[Char]): Parser[String] = {
    val chars = value.toList
    
    chars.headOption.map ({ head =>
      val tail = chars.tail
      
      tail.foldLeft(f(head).one) {
        case (acc, c) => acc <+> f(c).one
      }
    }).map(_.string).getOrElse(constant("")) ^! 
      (Doc.InlineText("Expected string '" + value + "'"), Doc.InlineText("Unexpected string '" + value + "'"))
  }

  // The range DSL permits much higher-quality parsers than simply filtering on any character
  object rangeDsl {
    case object CharParam {
      def == (value: Char) = CharEQ(value)

      def != (value: Char) = CharNEQ(value)
     
      def > (value: Char) = CharGT(value)

      def >= (value: Char) = CharGTE(value)

      def < (value: Char) = CharLT(value)

      def <= (value: Char) = CharLTE(value)
    }
    sealed trait CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char]

      def && (that: CharExp): CharExp = CharAnd(this, that)

      def || (that: CharExp): CharExp = CharOr(this, that)
    }  
    case class CharEQ(value: Char) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = f(value, value)
    }
    case class CharNEQ(value: Char) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = !f(value, value)
    }
    case class CharGT(value: Char) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = {
        if (value == Char.MaxValue) fail(Doc.InlineText("Unexpected character"), Doc.InlineText("Expected character"))
        else f((value + 1).toChar, Char.MaxValue)
      }
    }
    case class CharGTE(value: Char) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = f(value, Char.MaxValue)
    }
    case class CharLT(value: Char) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = {
        if (value == Char.MinValue) fail(Doc.InlineText("Unexpected character"), Doc.InlineText("Expected character"))
        else f((value - 1).toChar, value)
      }
    }
    case class CharLTE(value: Char) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = f(Char.MinValue, value)
    }
    case class CharAnd(left: CharExp, right: CharExp) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = left.parser(f) <&> right.parser(f)
    }
    case class CharOr(left: CharExp, right: CharExp) extends CharExp {
      def parser(f: (Char, Char) => Parser[Char]): Parser[Char] = left.parser(f) <|> right.parser(f)
    }
  }
}