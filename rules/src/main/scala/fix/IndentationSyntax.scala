package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured

case class IndentationSyntaxParameters(
  addEndMarkers: Boolean,
  blockSize: Option[Int] // if block size > N lines, add end marker
)

object IndentationSyntaxParameters {
  val default = IndentationSyntaxParameters(false, None)
  implicit val surface: metaconfig.generic.Surface[IndentationSyntaxParameters] = metaconfig.generic.deriveSurface[IndentationSyntaxParameters]
  implicit val decoder: metaconfig.ConfDecoder[IndentationSyntaxParameters] = metaconfig.generic.deriveDecoder(default)
}

class IndentationSyntax(params: IndentationSyntaxParameters)
    extends SyntacticRule("IndentationSyntax") {
  
  def this() = this(IndentationSyntaxParameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    config.conf.getOrElse("IndentationSyntax")(this.params).map(newParams => new IndentationSyntax(newParams))
  
  override def fix(implicit doc: SyntacticDocument): Patch = {
    def isLeftBrace(t: Token)   = t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
    def isRightBrace(t: Token)  = t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

    def isWhitespace(t: Token)  = t.isInstanceOf[scala.meta.tokens.Token.Whitespace]
    def isNewLine(t: Token)     = t.isInstanceOf[scala.meta.tokens.Token.EOL]
    def isSpace(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.HSpace]

    def isIndentation(t: Token) = t.isInstanceOf[scala.meta.tokens.Token.Indentation]
    def isIndent(t: Token)      = t.isInstanceOf[scala.meta.tokens.Token.Indentation.Indent]
    def isOutdent(t: Token)     = t.isInstanceOf[scala.meta.tokens.Token.Indentation.Outdent]
    
    doc.tree.collect {
      case block: Term.Block => 
        // the rule only applies to control structures
        val isInControlStructure = block.parent match {
          case Some(tree) => tree match {
            case Term.If(_, _, _) => true
            case Term.While(_, _) => true
            case Term.For(_, _) => true
            case Term.ForYield(_, _) => true
            case Term.Try(_, _, _) => true
            case _ => false
          }
          case None => false
        }

        // if there is no { on the same line as the start of the block,
        // assume that block is properly indented 
        // (according to the rules of significant indentation)
        // and we have nothing to do

        val isBracedBlock = block.tokens.takeWhile(t => !isNewLine(t)).exists(isLeftBrace)
        if (!isBracedBlock || !isInControlStructure) {
          Patch.empty
        } else {
          val leftBrace  = block.tokens.find(t => isLeftBrace(t)).get
          // val RightBrace = block.tokens.reverse.find(t => isRightBrace(t)).get
          // is it better to reverse and find, or to findLast?
          val rightBrace = block.tokens.findLast(t => isRightBrace(t)).get 

          // calculate the indentation level of the first line
          def isAfterLeftBrace(t: Token) = t.start >= leftBrace.end
          val firstIndentedToken = block.tokens.find(t => isAfterLeftBrace(t) && !isWhitespace(t)).get
          val newLineAfterLeftBrace = block.tokens.find(t => isAfterLeftBrace(t) && isNewLine(t)).get
          val firstIndentationLevel = firstIndentedToken.start - newLineAfterLeftBrace.end
          // println("Indentation level for this block:", firstIndentationLevel)

          // If there is no indentation (=0), set it to the indentation of the parent+2

          // match the indentation on all subsequent levels
          def isAfterFirstIndentedToken(t: Token) = t.start >= firstIndentedToken.end
          val newLineAfterFirstLine = block.tokens.find(t => isNewLine(t) && isAfterFirstIndentedToken(t)).get
          def isFromSecondLine(t: Token) = t.start >= newLineAfterFirstLine.end

          var tokensToIndent = block.tokens.filter(t => isFromSecondLine(t) && !isRightBrace(t))
          var patches: List[Patch] = Nil

          
          // while (!tokensToIndent.isEmpty) {
          while (tokensToIndent.exists(isNewLine)) {
            var (line, lines) = tokensToIndent.span(t => !isNewLine(t))
            // move the new line to the remaining tokens
            line  = line :+ lines.head
            lines = lines.tail

            val (whitespaceToRemove, tokensOnLine) = line.span(t => isSpace(t))
            patches = Patch.removeTokens(whitespaceToRemove) :: patches
            patches = Patch.addLeft(tokensOnLine.head, " " * firstIndentationLevel) :: patches

            tokensToIndent = lines
          }
          
          // block.tokens.takeWhile()

          // get lines: split the tokens on new lines (space+, tokens+, \n)
          // for every line in the block:
          // remove leading whitespace
          // put as many spaces as needed
        
          /* interesting booleans in dialects:
          dialects.Scala3.allowEndMarker
          dialects.Scala3.allowFewerBraces
          dialects.Scala3.allowMultilinePrograms
          dialects.Scala3.allowProcedureSyntax
          dialects.Scala3.allowSignificantIndentation
          dialects.Scala3.allowTypeInBlock
          */

          if (params.addEndMarkers) {
            // add END at the indentation level of the parent
            Patch.empty
          }

          val removeBraces = Patch.removeToken(leftBrace) + Patch.removeToken(rightBrace)

          // We assume that the last token in the block is the closing brace
          val whitespaceBeforeRightBrace = block.tokens.reverse.tail.takeWhile(isWhitespace)
          val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)

          patches.foldLeft(Patch.empty)(_ + _) + removeBraces + removeWhitespaceBeforeRightBrace
        }
        
    }.asPatch
  }
}
