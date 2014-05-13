package redeyes.api

import scalaz._

import redeyes.doc._
import redeyes.parser._

/**
 * The Api module contains the core functionality for describing Api.
 *
 * An Api is just a multi-channel, invertible parser. There exists a channel for
 * each component of a request / response: header, method, hash, path, and 
 * content.
 *
 * Ordinarily you won't use this module directly because the package object
 * includes this and other useful modules for dealing with the different channels.
 */
trait ApiModule extends ParserModule {
  protected sealed trait ApiSource

  protected type Channel = ApiSource
  protected object HeaderSource  extends Channel
  protected object MethodSource  extends Channel
  protected object QuerySource   extends Channel
  protected object HashSource    extends Channel
  protected object ContentSource extends Channel
  protected object PathSource    extends Channel

  protected implicit val ChannelEqual = new Equal[Channel] {
    def equal(v1: Channel, v2: Channel) = v1 == v2
  }

  protected sealed trait ApiAtoms[C <: Channel, A]
  protected type Atom[C <: Channel, A] = ApiAtoms[C, A]

  val charParser: CharParserModule

  type CParser[A] = charParser.Parser[A]
  type Api[A] = Parser[A]

  protected case class HeaderAtom[A](name: CParser[_], value: CParser[A]) extends Atom[HeaderSource.type, A]
  protected case class MethodAtom[A](name: CParser[A]) extends Atom[MethodSource.type, A]
  protected case class QueryAtom[A](name: CParser[_], value: CParser[A]) extends Atom[QuerySource.type, A]
  protected case class HashAtom[A](value: CParser[A]) extends Atom[HashSource.type, A]
  protected case class PathAtom[A](value: CParser[A]) extends Atom[PathSource.type, A]
  protected case class SmallContentAtom[A](value: CParser[A]) extends Atom[ContentSource.type, A]
  protected case class BigContentAtom[A](value: CParser[A]) extends Atom[ContentSource.type, A] // TODO: Change to Process

  def header[A](name: CParser[_], value: CParser[A]): Api[A] = atom(HeaderSource, HeaderAtom(name, value))

  def method[A](name: CParser[A]): Api[A] = atom(MethodSource, MethodAtom(name))

  def query[A](name: CParser[_], value: CParser[A]): Api[A] = atom(QuerySource, QueryAtom(name, value))

  def hash[A](value: CParser[A]): Api[A] = atom(HashSource, HashAtom(value))

  def path[A](value: CParser[A]): Api[A] = atom(PathSource, PathAtom(value))

  def content[A](value: CParser[A]): Api[A] = atom(ContentSource, SmallContentAtom(value))

  def attachment[A](value: CParser[A]): Api[A] = atom(ContentSource, BigContentAtom(value))

  protected val AtomAlgebra = new AtomAlgebra {
    def anything[C <: Channel](channel: C) = channel match {
      case HeaderSource  => HeaderAtom(charParser.anyChar, charParser.anyChar).asInstanceOf[Atom[C, _]]
      case MethodSource  => MethodAtom(charParser.anyChar).asInstanceOf[Atom[C, _]]
      case QuerySource   => QueryAtom(charParser.anyChar, charParser.anyChar).asInstanceOf[Atom[C, _]]
      case HashSource    => HashAtom(charParser.anyChar).asInstanceOf[Atom[C, _]]
      case PathSource    => PathAtom(charParser.anyChar).asInstanceOf[Atom[C, _]]
      case ContentSource => SmallContentAtom(charParser.anyChar).asInstanceOf[Atom[C, _]]
    }

    def negate[C <: Channel, A](atom: Atom[C, A]) : Parser[A] = atom match {
      case a @ HeaderAtom(_, _)     => header(a.name, !a.value) <|> header(a.name, !a.value) <|> header(!a.name, !a.value)
      case a @ MethodAtom(_)        => method(!a.name)
      case a @ QueryAtom(_, _)      => query(a.name, !a.value) <|> query(!a.name, a.value) <|> query(!a.name, !a.value)
      case a @ HashAtom(_)          => hash(!a.value)
      case a @ PathAtom(_)          => path(!a.value)
      case a @ SmallContentAtom(_)  => content(!a.value)
      case a @ BigContentAtom(_)    => attachment(!a.value)
    }

    def intersect[C <: Channel, A: Equal](a1: Atom[C, A], a2: Atom[C, A]): Parser[A] = ???
  }

  protected def generateAtom[C <: Channel, A](channel: C, atom: Atom[C, A]): EphemeralStream[A] = atom match {
    case HeaderAtom(name, value)  => charParser.generate(value)
    case MethodAtom(name)         => charParser.generate(name)
    case QueryAtom(name, value)   => charParser.generate(value)
    case HashAtom(value)          => charParser.generate(value)
    case PathAtom(value)          => charParser.generate(value)
    case SmallContentAtom(value)  => charParser.generate(value)
    case BigContentAtom(value)    => charParser.generate(value)
  }
}