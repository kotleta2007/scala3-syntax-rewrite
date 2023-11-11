package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured

case class RLEIndent(char: Char, count: Int) // check that char is strictly in {' ', '\t'} 

case class IndentationSyntaxParameters(
  addEndMarkers: Boolean,
  blockSize: Option[Int], // if block size > N lines, add end marker
  // insertEndMarkerMinLines (look at Scalafmt)
  // useOptimalIndentation (instead of first line indentation width, use smallest possible indentation (1 tab))
  // defaultIndentation: List[RLEIndent]
)

object IndentationSyntaxParameters {
  // val default = IndentationSyntaxParameters(false, None, List(RLEIndent(' ', 2)))
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

    def isHSpace(t: Token)      = t.isInstanceOf[scala.meta.tokens.Token.HSpace]       // Space, Tab
    def isNewLine(t: Token)     = t.isInstanceOf[scala.meta.tokens.Token.AtEOL]        // CR, LF, FF
    def isSpace(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.Space]        // Space
    def isWhitespace(t: Token)  = t.isInstanceOf[scala.meta.tokens.Token.Whitespace]   // HSpace, AtEOL
    
    // Map [Int, Indentation={tabs and spaces}]
    
    // var x = ???

    // look into scalatest/scalatest for code examples

    // look into scalaz
    // look into dotty/community-build/community-projects
    // look into dotty itself:  it's a mix of Scala2 and Scala3 syntax
    
    /*
    scala:

    * convert to lines
    * get whitespace from line (return RLE encoded list of tabs+spaces)
    * whitespace comparison operator < (2 tabs, 1 space < 2 tabs, 2 spaces)
    * write tests for nested (if and matching, while and try)
        1. control structures 2. try-catch and matching 3. templates
    * add end markers for the following types of blocks:
      "if", "try", "template"
    
    */
    
    doc.tree.collect {
      case template: Template => 
        val isBracedBlock = isLeftBrace(template.tokens.dropWhile(t => isWhitespace(t)).head)
        if (!isBracedBlock) {
          val addEndMarker = if (params.addEndMarkers) {
            val lastToken = template.tokens.last
            val indentationLevel = template.parent match {
              case Some(parentTree) => 
                // println("I have a parent")
                // println(parentTree)
                // println(template)
                def isColon(t: Token) = t.isInstanceOf[scala.meta.tokens.Token.Colon]
                val colon = parentTree.tokens.find(isColon).get
                def isAfterColon(t: Token) = t.start > colon.end
                val whitespaceAfterColon = template.tokens.filter(isAfterColon).takeWhile(isHSpace)
                val indentationLevelInParent = whitespaceAfterColon.size

                indentationLevelInParent
              case None => 
                val currentIndentationLevel = 0
                currentIndentationLevel
            }

            val newIndentation = indentationLevel - 2

            // scala.meta.Defn.Object]
            val templateName = template.parent.get match {
              case member: scala.meta.Member => member.name
            }
            val stringToAdd = "\n" + " " * newIndentation + "end " + templateName
            val endMarker = Patch.addRight(lastToken, stringToAdd)

            endMarker
          } else {
            Patch.empty
          }
          addEndMarker
        } else {
          val leftBrace  = template.tokens.find(t => isLeftBrace(t)).get
          val rightBrace = template.tokens.findLast(t => isRightBrace(t)).get

          // remove HSpace inside the template (between the "extends" clauses and the left brace)
          def isBeforeLeftBrace(t: Token) = t.end <= leftBrace.start
          val HSpaceBeforeLeftBrace = template.tokens.reverse.filter(t => isBeforeLeftBrace(t)).takeWhile(t => isHSpace(t) && isBeforeLeftBrace(t))
          val removeHSpaceBeforeLeftBrace = Patch.removeTokens(HSpaceBeforeLeftBrace)

          // remove HSpace between the ctor (constructor arguments) and the start of the template
          val removeHSpaceinParent = template.parent match {
            case Some(parentTree) =>
              def isBeforeTemplate(t: Token) = t.end <= template.pos.start
              val HSpaceBeforeTemplate = parentTree.tokens.reverse.filter(t => isBeforeTemplate(t)).takeWhile(t => isHSpace(t) && isBeforeTemplate(t))

              val removeHSpaceBeforeTemplate = Patch.removeTokens(HSpaceBeforeTemplate)
              
              removeHSpaceBeforeTemplate
            case None => Patch.empty
          }

          val replaceLeftBraceWithColon = Patch.replaceToken(leftBrace, ":")
          val removeRightBrace = Patch.removeToken(rightBrace)

          // We assume that the last token in the block is the closing brace
          val whitespaceBeforeRightBrace = template.tokens.reverse.tail.takeWhile(isHSpace)
          val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)

          removeHSpaceinParent + removeHSpaceBeforeLeftBrace + replaceLeftBraceWithColon + removeWhitespaceBeforeRightBrace + removeRightBrace
        }

      case block @ (_: Term.Block | _: Term.Try | _: Term.Match) => 
        // if we have a block (not cases), the rule only applies to control structures
        val isInControlStructure = if (!block.isInstanceOf[Term.Block]) true else block.parent match {
          case Some(tree) => tree match {
            case Term.If(_,_,_) | Term.While(_,_) => true
            case Term.For(_, _) => true
            case Term.ForYield(_, _) => true
            // add val, var, def
            case _ => false
          }
          case None => false
        }

        // if there is no { at the start of the block,
        // assume that block is properly indented 
        // (according to the rules of significant indentation)
        // and we have nothing to do

        // the first non-whitespace token after the start of the block is {
        val isBracedBlock = isLeftBrace(block.tokens.dropWhile(t => isWhitespace(t)).head)

        /*
        println("in block")
        println(block.toString())
        println("in control structure? : ", isInControlStructure)
        println("is braced block ? : ", isBracedBlock)
        */

        if (!isBracedBlock || !isInControlStructure) {
          Patch.empty

          // addEndMarker
        } else {
          val leftBrace  = block.tokens.find(t => isLeftBrace(t)).get
          val rightBrace = block.tokens.findLast(t => isRightBrace(t)).get

          // calculate the indentation level of the first line
          def isAfterLeftBrace(t: Token) = t.start >= leftBrace.end
          val firstIndentedToken = block.tokens.find(t => isAfterLeftBrace(t) && !isWhitespace(t)).get
          val newLineAfterLeftBrace = block.tokens.find(t => isAfterLeftBrace(t) && isNewLine(t)).get
          val firstIndentationLevel = firstIndentedToken.start - newLineAfterLeftBrace.end
          // println("Indentation level for this block:", firstIndentationLevel)

          // If there is no indentation (=0), set it to the indentation of the parent+1 indentation (2 spaces or tab)

          // match the indentation on all subsequent levels
          def isAfterFirstIndentedToken(t: Token) = t.start >= firstIndentedToken.end
          val newLineAfterFirstLine = block.tokens.find(t => isNewLine(t) && isAfterFirstIndentedToken(t)).get
          def isFromSecondLine(t: Token) = t.start >= newLineAfterFirstLine.end

          var tokensToIndent = block.tokens.filter(t => isFromSecondLine(t) && !isRightBrace(t))
          var patches: List[Patch] = Nil

          
          // while (!tokensToIndent.isEmpty) {
          while (tokensToIndent.exists(isNewLine)) {
            var (line, lines) = tokensToIndent.span(t => !isNewLine(t))
            // move the newLine to the tokens of the current line
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

          // Further work: use dialects for refining rule

          if (params.addEndMarkers) {
            // add END at the indentation level of the parent
            Patch.empty
          }

          val addEndMarker = Patch.empty
          /*
          val addEndMarker = params.addEndMarkers match {
            case true => 
              val endName = block match {
                case _: Term.If => "if"
                case _: Term.While => "while"
                case _: Term.For => "for"
                case _: Term.ForYield => "for"
                case _: Term.Try => "try"
                case _: Term.Match => "match"
              }
              Patch.addRight(rightBrace, "end " + endName)
            case false => Patch.empty 
          }
          */

          val removeBraces = Patch.removeToken(leftBrace) + Patch.removeToken(rightBrace)

          // We assume that the last token in the block is the closing brace
          val whitespaceBeforeRightBrace = block.tokens.reverse.tail.takeWhile(isWhitespace)
          val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)

          Patch.fromIterable(patches) + addEndMarker + removeWhitespaceBeforeRightBrace + removeBraces
        }
      }.asPatch
  }
}
