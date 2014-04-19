package redeyes.doc

import scalaz.{Cord, Monoid}
import scalaz.std.vector._

import Cord._

import scalaz.syntax.foldable._

trait Markdown {
  import Doc._
  
  private val NewlineMdRope: Cord     = "\n"
  private val OneTicMdRope: Cord      = "`"
  private val ThreeTicsMdRope: Cord   = "```"
  private val LeftBrackMdRope: Cord   = "["
  private val RightBrackMdRope: Cord  = "]"
  private val LeftParenMdRope: Cord   = "("
  private val RightParenMdRope: Cord  = ")"
  private val StarMdRope: Cord        = "*"
  
  def renderMarkdown(doc: Doc): String = {
    def renderMarkdown(sectionLevels: Int): (Doc => Cord) = (doc: Doc) => (doc match {
      case Empty => ""
      
      case Section(title, content) => 
        ("#" * sectionLevels + title) ++ NewlineMdRope ++ renderMarkdown(sectionLevels + 1)(content)
        
      case BlockRun(content) => content.foldMap(renderMarkdown(sectionLevels))
        
      case Table(header, content) => ???
      
      case BlockText(content) => 
        NewlineMdRope ++ content.foldMap(renderMarkdown(sectionLevels)) ++ NewlineMdRope
      
      case BlockCode(language, content) => 
        NewlineMdRope ++ ThreeTicsMdRope ++ language ++ NewlineMdRope ++ content ++ ThreeTicsMdRope ++ NewlineMdRope
      
      case OrderedList(content) => ???        
      
      case UnorderedList(content) => ???
      
      case InlineText(content) => content
      
      case InlineCode(language, content) => 
        OneTicMdRope ++ content ++ OneTicMdRope
      
      case InlineEmphasis(content) => 
        StarMdRope ++ content ++ StarMdRope
      
      case InlineLink(name, url) => 
        LeftParenMdRope ++ name ++ RightParenMdRope ++ LeftBrackMdRope ++ url ++ RightBrackMdRope
      
      case InlineRun(content) => 
        content.foldMap(renderMarkdown(sectionLevels))
    }): Cord
    
    renderMarkdown(1)(doc).toString
  }
  
  implicit class MarkdownDocWrapper(value: Doc) {
    def md: String = renderMarkdown(value)
  }
  
  implicit class MarkdownStringWrapper(value: String) {
    def md: Doc = ???
  }
}