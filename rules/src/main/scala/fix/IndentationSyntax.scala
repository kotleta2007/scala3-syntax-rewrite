package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured
import scala.annotation.tailrec
import scala.collection.mutable

case class IndentationSyntaxParameters(
  addEndMarkers: Boolean,
  skipEndMarkers: List[String],
  minLinesForEndMarker: Int,
  defaultIndentation: String,

  // When to Use End Markers
  // check docs!!!
  // for stuff like checking for empty lines, we have to implement linesOfTree to get the lines corresponding to the tree
  // then we will know whether there are empty lines and how many lines there are in total
)

object IndentationSyntaxParameters {
  val default = IndentationSyntaxParameters(false, Nil, 0, "  ")
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

    // isKeyword for end markers
    def isObject(t: Token)      = t.isInstanceOf[scala.meta.tokens.Token.KwObject]
    def isClass(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwClass]
    def isTrait(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwTrait]
    def isPackage(t: Token)     = t.isInstanceOf[scala.meta.tokens.Token.KwPackage]
    def isExtension(t: Token)   = t.isInstanceOf[scala.meta.tokens.Token.Ident] && t.text == "extension"
    
    // mod or case
    def isMod(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.ModifierKeyword] || t.isInstanceOf[scala.meta.tokens.Token.KwCase]

    def isVal(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwVal]
    def isVar(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwVar]
    def isDef(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwDef]
    
    def isGiven(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwGiven]

    def isThis(t: Token)        = t.isInstanceOf[scala.meta.tokens.Token.KwThis]
    def isNew(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwNew]
    
    
    // WHAT'S A "PACKAGE OBJECT" ???
    def isIf(t: Token)          = t.isInstanceOf[scala.meta.tokens.Token.KwIf]
    def isWhile(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwWhile]
    def isTry(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwTry]

    def isIdentifier(t: Token, name: String) = t.isInstanceOf[scala.meta.tokens.Token.Ident] && t.text == name

    def endMarkerSkipped(tree: Tree): Boolean = {
      tree match {
        case _: Defn.Object       => params.skipEndMarkers.contains("object")
        case _: Defn.Class        => params.skipEndMarkers.contains("class")
        case _: Defn.Trait        => params.skipEndMarkers.contains("trait")

        case _: Term.If           => params.skipEndMarkers.contains("if")
        case _: Term.While        => params.skipEndMarkers.contains("while")
        case _: Term.For          => params.skipEndMarkers.contains("for")
        case _: Term.ForYield     => params.skipEndMarkers.contains("forYield")
        case _: Term.Match        => params.skipEndMarkers.contains("match")
        case _: Term.Try          => params.skipEndMarkers.contains("try")
        case _: Term.NewAnonymous => params.skipEndMarkers.contains("new")
        case _: Ctor.Secondary    => params.skipEndMarkers.contains("this")
        case _: Defn.Val          => params.skipEndMarkers.contains("val")
        case _: Defn.Var          => params.skipEndMarkers.contains("var")
        case _: Defn.Def          => params.skipEndMarkers.contains("def")
        
        case _: Defn.GivenAlias => params.skipEndMarkers.contains("given")

        case _                  => false
      }
    }

    def addEndMarkerMethod(tree: Tree): Patch = {
      if (!params.addEndMarkers || endMarkerSkipped(tree)) {
        Patch.empty
      } else {
        val lastToken = tree.tokens.last

        // if tree already has an end marker, don't add it!
        // ideally, we wouldn't have to check for this

        val lookFor = tree match {
          case defn: Stat.WithMods if !defn.mods.isEmpty => isMod _ 
          case _: Defn.Object                            => isObject _
          case _: Defn.Class                             => isClass _
          case _: Defn.Trait                             => isTrait _
          case _: Defn.ExtensionGroup                    => isExtension _
          case _: Term.If                                => isIf _
          case _: Term.While                             => isWhile _
          case _: Term.Try                               => isTry _

          case _: Defn.Val                               => isVal _
          case _: Defn.Var                               => isVar _
          case _: Defn.Def                               => isDef _
          
          case _: Defn.GivenAlias                        => isGiven _
          case _: Term.NewAnonymous                      => isNew _
          case _: Ctor.Secondary                         => isDef _ // secondary ctor definition starts with the "def" keyword
          case matchTerm: Term.Match                     => isIdentifier(_, matchTerm.expr.toString()) // match expression starts with an identifier
          
          case _                                         => throw new NotImplementedError("End markers for this syntactic structure are not implemented.")
        }

        val keyword = tree.tokens.find(lookFor).getOrElse(throw new Exception("The given tree doesn't contain its defining keyword."))
        def isBeforeKeyword(t: Token) = t.end <= keyword.start
        val whitespaceBeforeKeyword = tree.tokens.tokens.filter(isBeforeKeyword).reverse.takeWhile(isHSpace)
        val indentationLevel = whitespaceBeforeKeyword.size

        val endMarkerName = tree match {
          case _: Term.If => "if"
          case _: Term.While => "while"
          case _: Term.For => "for"
          case _: Term.ForYield => "for"
          case _: Term.Match => "match"
          
          case _: Term.Try => "try"
          case _: Term.NewAnonymous => "new"
          // case _: Term.This => "this"
          
          case valTree: Defn.Val => valTree.pats.head match {
            case pat: Pat.Var => pat.name.value
            case _            => "val"
          }

          case varTree: Defn.Var => varTree.pats.head match {
            case pat: Pat.Var => pat.name.value
            case _            => "var"
          }
          
          case givenTree: Defn.GivenAlias => givenTree.name match {
            case _: Name.Anonymous => "given"
            case n: Name => n.value
          }

          case _: Defn.ExtensionGroup => "extension"

          case defTree: Defn.Def => defTree.name match {
            case _: Name.Anonymous => "def"
            case n: Name => n.value
          }

          // case _: Ctor.Secondary => "this"
          case ctorTree: Ctor.Secondary => ctorTree.name match {
            case _: Name.This => "this"
            case n: Name => n.value
          }
          
          case member: scala.meta.Member => member.name // test if it returns THIS for secondary constructor
        }

        val stringToAdd = "\n" + " " * indentationLevel + "end " + endMarkerName
        val endMarker = Patch.addRight(lastToken, stringToAdd)

        endMarker
      }
    }

    sealed trait IndentationCharacter

    case object Space extends IndentationCharacter
    case object Tab extends IndentationCharacter

    // type RLEIndent = List[(Int, IndentationCharacter)]
    case class RLEIndent(indents: List[(Int, IndentationCharacter)]) {
      def < (that: RLEIndent): Boolean = {
        (this, that) match {
          case (RLEIndent(Nil), RLEIndent(Nil))                                               => false
          case (RLEIndent(Nil), RLEIndent(h2 :: t2))                                          => true
          case (RLEIndent(h1 :: t1), RLEIndent(Nil))                                          => false
          case (RLEIndent(h1 :: t1), RLEIndent(h2 :: t2)) if h1._2 == h2._2 && h1._1 < h2._1  => RLEIndent(t1) <= RLEIndent(t2)
          case (RLEIndent(h1 :: t1), RLEIndent(h2 :: t2)) if h1._2 == h2._2 && h1._1 == h2._1 => RLEIndent(t1) < RLEIndent(t2)
          case _                                                                              => false
        }
      }

      @scala.annotation.tailrec
      def == (that: RLEIndent): Boolean = {
        (this, that) match {
          case (RLEIndent(Nil), RLEIndent(Nil))                       => true
          case (RLEIndent(h1 :: t1), RLEIndent(h2 :: t2)) if h1 == h2 => RLEIndent(t1) == RLEIndent(t2)
          case _                                                      => false
        }
      }

      def <= (that: RLEIndent): Boolean = this < that || this == that
      def != (that: RLEIndent): Boolean = !(this == that)
      def > (that: RLEIndent):  Boolean = !(this <= that)
      def >= (that: RLEIndent): Boolean = !(this < that)
    }

    def rleFromTokens(tokens: Seq[Token]): RLEIndent = {
      def pack[T](xs: Seq[T]): List[List[T]] = {
        xs match {
          case Nil => Nil
          case head :: next =>
            val (same, rest) = next.span(_ == head)
            (head :: same) :: pack(rest)
        }
      }

      def convertToIndentationCharacter(token: scala.meta.tokens.Token): IndentationCharacter = {
        token match {
          case _: scala.meta.tokens.Token.Space => Space
          case _: scala.meta.tokens.Token.Tab   => Tab
          case _                                => throw new Exception("The given token is not HSpace (not a space or a tab).")
        }
      }

      val indents = pack(tokens.toList.map(convertToIndentationCharacter)).map(l => (l.size, l.head))

      RLEIndent(indents)
    }

    def rleFromString = ???

    @tailrec
    def splitOnTail[T](l: Seq[T], acc: Seq[Seq[T]], predicate: T => Boolean): Seq[Seq[T]] = {
      val (left, right) = l.span((n: T) => !predicate(n))
      if (right.isEmpty) (left +: acc).reverse
      else splitOnTail(right.tail, (left :+ right.head) +: acc, predicate)
    }
    
    // maybe use collection.mutable.Seq ???
    def splitOn(l: Seq[Token], delimiter: String) = {
      val split = splitOnTail(l, Seq.empty, ((x: Token) => x.text == delimiter))

      if (split.last == Seq.empty) split.init else split
    }

    // subset of lines containing given tree
    // def linesFromTree(t: Tree)

    // CHECK IF-THEN-ELSE, both for indentation and 
    /*
    val lines: DocLines: List[Array[Token]] = ??? // compute lines

    val innerToken: Tree = ???
    val line: Int = lines.get(innerToken)
    val indent: RLEIndent = lines.getIndent(line)


    var patches = Nil
    */
    val linesByToken: Map[Token, Int] = ???
    val oldIndentationByLine: Seq[RLEIndent] = ???
    val newIndentationByLine: mutable.Seq[RLEIndent] = mutable.Seq(oldIndentationByLine: _*)

    val docLines: Seq[Seq[Token]] = splitOn(doc.tree.tokens.tokens, "\n")
    val indentedLine = docLines.map(line => {
      val (whitespace, remainingTokens) = line.span(isHSpace)

      (rleFromTokens(whitespace), remainingTokens)
    })
    // docLines.foreach(line => line.foreach(token => println(s"\"$token\", ${token.getClass().getCanonicalName()}, ${isHSpace(token)}")))
    indentedLine.foreach(println)

    val endMarkerPatches: List[Patch] = doc.tree.collect {
      // end markers
      case controlStructureTree @ (_: Term.If | _: Term.While | _: Term.Try | _: Term.Match) => 
        addEndMarkerMethod(controlStructureTree)
      case defnTree @ (_: Defn.Val | _: Defn.Var | _: Defn.Def | _: Defn.GivenAlias | _: Term.NewAnonymous) => 
        addEndMarkerMethod(defnTree)
      case containingTemplateTree @ (_: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.ExtensionGroup | _: Ctor.Secondary) => 
        addEndMarkerMethod(containingTemplateTree)
    }.reverse

    val bracePatches = doc.tree.collect {
      case template: Template => 
        val isBracedBlock = isLeftBrace(template.tokens.dropWhile(t => isWhitespace(t)).head)
        if (!isBracedBlock) {
          Patch.empty
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
        // later can be removed
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
        val isBracedBlock = !block.tokens.isEmpty && isLeftBrace(block.tokens.dropWhile(t => isWhitespace(t)).head)

        if (!isBracedBlock || !isInControlStructure) {
          Patch.empty
        } else {
          val leftBrace  = block.tokens.find(t => isLeftBrace(t)).get
          val rightBrace = block.tokens.findLast(t => isRightBrace(t)).get

          // calculate the indentation level of the first line
          def isAfterLeftBrace(t: Token) = t.start >= leftBrace.end
          block.parent.get.tokens.head
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

          val removeBraces = Patch.removeToken(leftBrace) + Patch.removeToken(rightBrace)

          // We assume that the last token in the block is the closing brace
          val whitespaceBeforeRightBrace = block.tokens.reverse.tail.takeWhile(isWhitespace)
          val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)

          Patch.fromIterable(patches) + removeWhitespaceBeforeRightBrace + removeBraces
        }
      }

    val computeIndentationPatches = doc.tree.collect {
      case _ => Patch.empty
    }
      
    bracePatches.asPatch + endMarkerPatches.asPatch ++ computeIndentationPatches
  }

}
