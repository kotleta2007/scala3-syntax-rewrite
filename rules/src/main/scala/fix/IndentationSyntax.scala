package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured
import scala.annotation.tailrec
import scala.collection.mutable
import com.google.protobuf.Empty

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
    /*
     * START OF END MARKERS
     */
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
    /*
     * END OF END MARKERS
     */

    /*
     * START OF RLE INDENT
     */
    sealed trait IndentationCharacter

    case object Space extends IndentationCharacter
    case object Tab extends IndentationCharacter

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

    def rleFromString(s: String): RLEIndent = {
      def pack[T](xs: Seq[T]): List[List[T]] = {
        xs match {
          case Nil => Nil
          case head :: next =>
            val (same, rest) = next.span(_ == head)
            (head :: same) :: pack(rest)
        }
      }

      def convertToIndentationCharacter(character: Char): IndentationCharacter = {
        character match {
          case _: ' '  => Space
          case _: '\t' => Tab
          case _       => throw new Exception("The given character is not HSpace (not a space or a tab).")
        }
      }

      val indents = pack(s.toList.map(convertToIndentationCharacter)).map(l => (l.size, l.head))

      RLEIndent(indents)
    }

    val defaultIndentation = rleFromString(params.defaultIndentation)
    /*
     * END OF RLE INDENT
     */

    /*
     * START OF SPLITTING INTO LINES (INDENTS AND TOKENS)
     */
    @tailrec
    def splitOnTail[T](l: Seq[T], acc: Seq[Seq[T]], predicate: T => Boolean): Seq[Seq[T]] = {
      val (left, right) = l.span((n: T) => !predicate(n))
      if (right.isEmpty) (left +: acc).reverse
      else splitOnTail(right.tail, (left :+ right.head) +: acc, predicate)
    }
    
    def splitOn(l: Seq[Token], delimiter: String) = {
      val split = splitOnTail(l, Seq.empty, ((x: Token) => x.text == delimiter))

      if (split.last == Seq.empty) split.init else split
    }
    
    val docLines: Seq[Seq[Token]] = splitOn(doc.tree.tokens.tokens, "\n")
    val indentsAndTokens = docLines.map(line => {
      val (whitespace, remainingTokens) = line.span(isHSpace)

      (rleFromTokens(whitespace), remainingTokens)
    })

    // println("*** START OF NEW DOCUMENT ***")
    // indentsAndTokens.foreach(println)
    /*
     * END OF SPLITTING INTO LINES (INDENTS AND TOKENS)
     */

    def linesFromTree(t: Tree) = {
      // returns subset of lines containing given tree
      val (from, to) = (t.tokens.head, t.tokens.last)
      val firstLine = docLines.indexWhere(_ == from)
      val lastLine = docLines.lastIndexWhere(_ == to)

      docLines.slice(firstLine, lastLine + 1)
    }

    /*
    val lines: DocLines: List[Array[Token]] = ??? // compute lines

    val innerToken: Tree = ???
    val line: Int = lines.get(innerToken)
    val indent: RLEIndent = lines.getIndent(line)

    var patches = Nil
    */

    // val linesByToken: Map[Token, Int] = ???
    // val oldIndentationByLine: Seq[RLEIndent] = ???
    // val newIndentationByLine: mutable.Seq[RLEIndent] = mutable.Seq(oldIndentationByLine: _*)

    /*
     * START PATCHES
     */
    val endMarkerPatches: List[Patch] = doc.tree.collect {
      case controlStructureTree @ (_: Term.If | _: Term.While | _: Term.Try | _: Term.Match) => 
        addEndMarkerMethod(controlStructureTree)
      case defnTree @ (_: Defn.Val | _: Defn.Var | _: Defn.Def | _: Defn.GivenAlias | _: Term.NewAnonymous) => 
        addEndMarkerMethod(defnTree)
      case containingTemplateTree @ (_: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.ExtensionGroup | _: Ctor.Secondary) => 
        addEndMarkerMethod(containingTemplateTree)
    }.reverse

    // TODO: think of something more clever
    def isBraced(t: Tree) = t.tokens.exists(isLeftBrace) && t.tokens.exists(isRightBrace)

    val bracePatches = doc.tree.collect {
      case template: Template if isBraced(template) =>
        val leftBrace = template.parent.get.tokens.find(isLeftBrace).get
        val rightBrace = template.parent.get.tokens.findLast(isRightBrace).get

        def isBeforeLeftBrace(t: Token) = t.end <= leftBrace.start
        def isBeforeRightBrace(t: Token) = t.end <= rightBrace.start
        
        val tokensBeforeLeftBrace = template.parent.get.tokens.takeWhile(isBeforeLeftBrace)
        val whitespaceBeforeLeftBrace = tokensBeforeLeftBrace.takeRightWhile(isWhitespace)
        val tokensBeforeRightBrace = template.parent.get.tokens.takeWhile(isBeforeRightBrace)
        val whitespaceBeforeRightBrace = tokensBeforeRightBrace.takeRightWhile(isWhitespace)

        val removeWhitespaceBeforeLeftBrace = Patch.removeTokens(whitespaceBeforeLeftBrace)
        val replaceLeftBraceWithColon = Patch.replaceToken(leftBrace, ":")
        val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)
        val removeRightBrace = Patch.removeToken(rightBrace)

        removeWhitespaceBeforeLeftBrace + replaceLeftBraceWithColon + removeWhitespaceBeforeRightBrace + removeRightBrace

      case block: Term.Block if isBraced(block) => 
        val leftBrace = block.parent.get.tokens.find(isLeftBrace).get
        val rightBrace = block.parent.get.tokens.findLast(isRightBrace).get

        def isBeforeLeftBrace(t: Token) = t.end <= leftBrace.start
        def isBeforeRightBrace(t: Token) = t.end <= rightBrace.start
        
        val tokensBeforeLeftBrace = block.parent.get.tokens.takeWhile(isBeforeLeftBrace)
        val whitespaceBeforeLeftBrace = tokensBeforeLeftBrace.takeRightWhile(isWhitespace)
        val tokensBeforeRightBrace = block.parent.get.tokens.takeWhile(isBeforeRightBrace)
        val whitespaceBeforeRightBrace = tokensBeforeRightBrace.takeRightWhile(isWhitespace)

        val removeWhitespaceBeforeLeftBrace = Patch.removeTokens(whitespaceBeforeLeftBrace)
        val removeLeftBrace = Patch.removeToken(leftBrace)
        val removeWhitespaceBeforeRightBrace = Patch.removeTokens(whitespaceBeforeRightBrace)
        val removeRightBrace = Patch.removeToken(rightBrace)

        removeWhitespaceBeforeLeftBrace + removeLeftBrace + removeWhitespaceBeforeRightBrace + removeRightBrace
      
      case _ => Patch.empty
    }

    val computeIndentationPatches = doc.tree.collect {
      case _ => Patch.empty
    }
    
    bracePatches.asPatch + endMarkerPatches.asPatch ++ computeIndentationPatches
    /*
     * END PATCHES
     */
  }

  /*
   * START OF HELPER FUNCTIONS (TOKEN PREDICATES)
   */
  def isLeftBrace(t: Token)   = t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
  def isRightBrace(t: Token)  = t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

  def isHSpace(t: Token)      = t.isInstanceOf[scala.meta.tokens.Token.HSpace]       // Space, Tab
  def isNewLine(t: Token)     = t.isInstanceOf[scala.meta.tokens.Token.AtEOL]        // CR, LF, FF
  def isSpace(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.Space]        // Space
  def isWhitespace(t: Token)  = t.isInstanceOf[scala.meta.tokens.Token.Whitespace]   // HSpace, AtEOL

  def isObject(t: Token)      = t.isInstanceOf[scala.meta.tokens.Token.KwObject]
  def isClass(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwClass]
  def isTrait(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwTrait]
  def isPackage(t: Token)     = t.isInstanceOf[scala.meta.tokens.Token.KwPackage]
  def isExtension(t: Token)   = t.isInstanceOf[scala.meta.tokens.Token.Ident] && t.text == "extension"
  def isMod(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.ModifierKeyword] || t.isInstanceOf[scala.meta.tokens.Token.KwCase]
  def isVal(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwVal]
  def isVar(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwVar]
  def isDef(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwDef]
  def isGiven(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwGiven]
  def isThis(t: Token)        = t.isInstanceOf[scala.meta.tokens.Token.KwThis]
  def isNew(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwNew]
  def isIf(t: Token)          = t.isInstanceOf[scala.meta.tokens.Token.KwIf]
  def isWhile(t: Token)       = t.isInstanceOf[scala.meta.tokens.Token.KwWhile]
  def isTry(t: Token)         = t.isInstanceOf[scala.meta.tokens.Token.KwTry]
  def isIdentifier(t: Token, name: String) = t.isInstanceOf[scala.meta.tokens.Token.Ident] && t.text == name
  /*
   * END OF HELPER FUNCTIONS (TOKEN PREDICATES)
   */

  /* a note regarding templates and package objects

  A template defines the type signature, behavior and initial state of a trait or class of objects or of a single object. 
  https://www.scala-lang.org/files/archive/spec/3.4/05-classes-and-objects.html#templates

  Also, package objects in Scalameta:
  https://scalameta.org/docs/trees/quasiquotes.html
  */
}
