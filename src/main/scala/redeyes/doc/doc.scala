package redeyes.doc

import scalaz.Monoid

sealed trait Doc {
  import Doc._
  
  def toBlock: Block
  
  def :+ (that: Doc): Doc = (this, that) match {
    case (Empty, _) => that
    case (_, Empty) => this
    
    case (BlockRun(content1), BlockRun(content2)) => BlockRun(content1 ++ content2)
    
    case (BlockRun(content1), that : Block) => BlockRun(content1 :+ that)
    case (BlockRun(content1), that : Inline) => BlockRun(content1 :+ BlockText(Vector(that)))
    
    case (this0 : Block, BlockRun(content2)) => BlockRun(this0 +: content2)
    case (this0 : Inline, BlockRun(content2)) => BlockRun(BlockText(Vector(this0)) +: content2)
    
    case (InlineRun(content1), InlineRun(content2)) => InlineRun(content1 ++ content2)
    
    case (InlineRun(content1), that : Inline) => InlineRun(content1 :+ that)
    
    case (this0 : Inline, InlineRun(content2)) => InlineRun(this0 +: content2)

    case (this0 : Block, that : Block) => BlockRun(Vector(this0, that))
    
    case (this0 : Block, that : Inline) => BlockRun(Vector(this0, that.toBlock))
    
    case (this0 : Inline, that : Block) => BlockRun(Vector(this0.toBlock, that))
    
    case (this0 : Inline, that : Inline) => InlineRun(Vector(this0, that))
  }
}

trait DocInstances {
  implicit def DocumentDocumented[A <: Doc]: Documented[A] = new Documented[A] {
    def document(value: A) = value
  }

  implicit val DocMonoid = new Monoid[Doc] {
    def zero = Doc.Empty

    def append(v1: Doc, v2: => Doc) = v1 :+ v2
  }
}

object Doc extends DocInstances {
  sealed trait Inline extends Doc
  sealed trait Block extends Doc {
    def toBlock = this
  }

  case object Empty extends Inline with Block

  final case class Section(title: String, content: Doc) extends Block
  final case class BlockRun(content: Vector[Block]) extends Block

  final case class Th(content: String, align: Align)
  final case class Table(header: Vector[Th], content: Vector[Vector[Doc]]) extends Block

  final case class BlockText(content: Vector[Inline]) extends Block
  final case class BlockCode(language: String, content: String) extends Block

  final case class OrderedList(content: Vector[Doc]) extends Block
  final case class UnorderedList(content: Vector[Doc]) extends Block

  final case class InlineText(content: String) extends Inline {
    def toBlock = BlockText(Vector[Inline](this))
  }
  final case class InlineCode(language: String, content: String) extends Inline {
    def toBlock = BlockCode(language, content)
  }
  final case class InlineEmphasis(content: String) extends Inline {
    def toBlock = BlockText(Vector[Inline](this))
  }
  final case class InlineLink(name: String, url: String) extends Inline {
    def toBlock = BlockText(Vector[Inline](this))
  }
  final case class InlineRun(content: Vector[Inline]) extends Inline {
    def toBlock = BlockText(Vector[Inline](this))
  }

  sealed trait Align
  object Align {
    case object Left extends Align
    case object Right extends Align
    case object Center extends Align
  }
}

trait Documented[A] {
  def document(value: A): Doc
}

object Documented {
  @inline
  final def apply[A](implicit d: Documented[A]) = d
}