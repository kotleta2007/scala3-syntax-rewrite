package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured
import scala.annotation.tailrec
import scala.collection.mutable
import com.google.protobuf.Empty

case class AddEndMarkersParameters(
  addEndMarkers: Boolean,
  skipEndMarkers: List[String],
  minLinesForEndMarker: Int,
  defaultIndentation: String,
)

object AddEndMarkersParameters {
  val default = AddEndMarkersParameters(false, Nil, 0, "  ")
  implicit val surface: metaconfig.generic.Surface[AddEndMarkersParameters] = metaconfig.generic.deriveSurface[AddEndMarkersParameters]
  implicit val decoder: metaconfig.ConfDecoder[AddEndMarkersParameters] = metaconfig.generic.deriveDecoder(default)
}

class AddEndMarkers(params: AddEndMarkersParameters)
    extends SyntacticRule("AddEndMarkers") {
  
  def this() = this(AddEndMarkersParameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    config.conf.getOrElse("AddEndMarkers")(this.params).map(newParams => new AddEndMarkers(newParams))
  
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
        case _: Defn.Enum         => params.skipEndMarkers.contains("enum")
        
        case _: Defn.GivenAlias => params.skipEndMarkers.contains("given")

        case _: Pkg.Object        => params.skipEndMarkers.contains("packageObject")

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
          case _: Defn.Enum                              => isEnum _
          
          case _: Defn.GivenAlias                        => isGiven _
          case _: Term.NewAnonymous                      => isNew _
          case _: Ctor.Secondary                         => isDef _ // secondary ctor definition starts with the "def" keyword
          case matchTerm: Term.Match                     => isIdentifier(_, matchTerm.expr.toString()) // match expression starts with an identifier
          
          case _: Pkg.Object                             => isPackage _

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

          case enumTree: Defn.Enum => enumTree.name match {
            case _: Name.Anonymous => "enum"
            case n: Name => n.value
          }

          // case _: Ctor.Secondary => "this"
          case ctorTree: Ctor.Secondary => ctorTree.name match {
            case _: Name.This => "this"
            case n: Name => n.value
          }

          case pkgObjectTree: Pkg.Object => pkgObjectTree.name match {
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
     * START PATCHES
     */
    val endMarkerPatches: List[Patch] = doc.tree.collect {
      case controlStructureTree @ (_: Term.If | _: Term.While | _: Term.Try | _: Term.Match) => 
        addEndMarkerMethod(controlStructureTree)
      case defnTree @ (_: Defn.Val | _: Defn.Var | _: Defn.Def | _: Defn.Enum | _: Defn.GivenAlias | _: Term.NewAnonymous) => 
        addEndMarkerMethod(defnTree)
      case containingTemplateTree @ (_: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.ExtensionGroup | _: Ctor.Secondary | _: Pkg.Object) => 
        addEndMarkerMethod(containingTemplateTree)
    }.reverse

    endMarkerPatches.asPatch
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
  def isEnum(t: Token)        = t.isInstanceOf[scala.meta.tokens.Token.KwEnum]
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