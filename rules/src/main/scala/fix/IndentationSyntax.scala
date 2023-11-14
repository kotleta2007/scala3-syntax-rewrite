package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured
import scala.annotation.tailrec

case class IndentationSyntaxParameters(
  addEndMarkers: Boolean,
  endMarkers: List[String]

  /* End markers supported:
    - object
    - class
    - if
  */
)

object IndentationSyntaxParameters {
  val default = IndentationSyntaxParameters(false, List("object", "if"))
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
    
    sealed trait IndentationCharacter

    case object Empty extends IndentationCharacter
    case object Space extends IndentationCharacter
    case object Tab extends IndentationCharacter

    // type RLEIndent = List[(Int, IndentationCharacter)]
    case class RLEIndent(indents: List[(Int, IndentationCharacter)])

    // val trial = RLEIndent(List((2, Tab), (4, Space), (1, Empty)))

    @tailrec
    def splitOnTail[T](l: Seq[T], acc: Seq[Seq[T]], predicate: T => Boolean): Seq[Seq[T]] = {
      val (left, right) = l.span((n: T) => !predicate(n))
      if (right.isEmpty) (left +: acc).reverse
      else splitOnTail(right.tail, (left :+ right.head) +: acc, predicate)
    }
      
    def splitOn[T](l: Seq[T], delimiter: T) = {
      val split = splitOnTail(l, Seq.empty, ((x: T) => x == delimiter))

      if (split.last == Seq.empty) split.init else split
    }


    def matchTreeTypeWithEndMarkers(tree: Tree): Boolean = {
      tree match {
        case _: Defn.Object => params.endMarkers.contains("object")
        case _: Defn.Class  => params.endMarkers.contains("class")
        case _: Term.If => params.endMarkers.contains("if")

        case _ => false
      }
    }

    def addEndMarkerMethod(tree: Tree): Patch = {
      if (!params.addEndMarkers || !matchTreeTypeWithEndMarkers(tree)) {
        Patch.empty
      } else {
        val lastToken = tree.tokens.last

        // if tree already has an end marker, don't add it!

        // val matchIndentationOf = ???

        val indentationLevel = tree match {
          case objectTree: Defn.Object => 
            def isObject(t: Token) = t.isInstanceOf[scala.meta.tokens.Token.KwObject]
            val objectKw = objectTree.tokens.find(isObject).get
            def isBeforeObject(t: Token) = t.end <= objectKw.start

            val whitespaceAfterObject = objectTree.tokens.tokens.filter(isBeforeObject).reverse.takeWhile(isHSpace)
            val indentationLevelInParent = whitespaceAfterObject.size

            indentationLevelInParent
          case ifTree: Term.If => 
            def isIf(t: Token) = t.isInstanceOf[scala.meta.tokens.Token.KwIf]
            val ifKw = ifTree.tokens.find(isIf).get
            def isBeforeIf(t: Token) = t.end <= ifKw.start

            val whitespaceAfterIf = ifTree.tokens.tokens.filter(isBeforeIf).reverse.takeWhile(isHSpace)
            val indentationLevelInParent = whitespaceAfterIf.size

            indentationLevelInParent
          case _ => 
            throw new NotImplementedError("End markers for this structure are not implemented.")
        }

        val endMarkerName = tree match {
          case member: scala.meta.Member => member.name
          case _: Term.If => "if"
        }

        val stringToAdd = "\n" + " " * indentationLevel + "end " + endMarkerName
        val endMarker = Patch.addRight(lastToken, stringToAdd)

        endMarker
      }
    }

    // CHECK IF-THEN-ELSE

    doc.tree.collect {
      case template: Template => 
        val isBracedBlock = isLeftBrace(template.tokens.dropWhile(t => isWhitespace(t)).head)
        if (!isBracedBlock) {
          addEndMarkerMethod(template.parent.get)
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

        if (!isBracedBlock || !isInControlStructure) {
          addEndMarkerMethod(block.parent.get)
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

          val addEndMarker = addEndMarkerMethod(block.parent.get)

          val removeBraces = Patch.removeToken(leftBrace) + Patch.removeToken(rightBrace)

          // We assume that the last token in the block is the closing brace
          val whitespaceBeforeRightBrace = block.tokens.reverse.tail.takeWhile(isWhitespace)
          val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)

          Patch.fromIterable(patches) + addEndMarker + removeWhitespaceBeforeRightBrace + removeBraces
        }
      }.asPatch
  }
}
