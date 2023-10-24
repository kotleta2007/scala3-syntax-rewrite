package fix

import scalafix.v1._
import scala.meta._
import scala.meta.tokens.Token.LeftParen
import metaconfig.Configured

case class Scala2ControlSyntaxParameters(
  useCatchInlining: Boolean
)

object Scala2ControlSyntaxParameters {
  val default = Scala2ControlSyntaxParameters(false)
  implicit val surface: metaconfig.generic.Surface[Scala2ControlSyntaxParameters] = metaconfig.generic.deriveSurface[Scala2ControlSyntaxParameters]
  implicit val decoder: metaconfig.ConfDecoder[Scala2ControlSyntaxParameters] = metaconfig.generic.deriveDecoder(default)
}

class Scala2ControlSyntax(params: Scala2ControlSyntaxParameters)
    extends SyntacticRule("Scala2ControlSyntax") {
  
  def this() = this(Scala2ControlSyntaxParameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    config.conf.getOrElse("Scala2ControlSyntax")(this.params).map(newParams => new Scala2ControlSyntax(newParams))
  
  override def fix(implicit doc: SyntacticDocument): Patch = {
    val isLeftParen  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
    val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]
    
    val isLeftBrace  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
    val isRightBrace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

    val isWhitespace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.Whitespace]
    val isNewLine    = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.EOL]

    val isCatch = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCatch]
    val isCase  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCase]
    val isThen  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwThen]
    val isDo    = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwDo]
    val isTry   = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwTry]

    doc.tree.collect {
      // Rule 1.1
      case ifTree: Term.If =>
        val condStart = ifTree.cond.tokens.head.start
        val condEnd   = ifTree.cond.tokens.last.end
        val thenBegin = ifTree.thenp.tokens.head.start
        
        def isBeforeCond(t: Token) = t.start < condStart
        def isAfterCond(t: Token)  = t.start >= condEnd && t.end < thenBegin

        // get first opening parenthesis
        val leftParen = ifTree.tokens.find(t => isLeftParen(t) && isBeforeCond(t))

        // get first closing parenthesis after condition
        val rightParen = ifTree.tokens.find(t => isRightParen(t) && isAfterCond(t))
        
        // add parentheses if necessary
        val addParens = (leftParen, rightParen) match {
          case (Some(_), Some(_)) => Patch.empty
          case (None, None) => Patch.addAround(ifTree.cond, "(", ")")
          case _ => throw new IllegalArgumentException("Invalid syntax: unmatched parentheses")
        }

        // remove THEN keyword if necessary
        val thenToken = ifTree.tokens.find(isThen)
        
        val removeThen = thenToken match {
          case Some(t) => 
            val spaceBeforeThen = ifTree.tokens.find(_.start == t.start - 1).get
            
            Patch.removeTokens(List(spaceBeforeThen, t))
          case None => Patch.empty
        }

        addParens + removeThen

      // Rule 1.2
      case whileTree: Term.While => 
        val exprStart = whileTree.expr.tokens.head.start
        val exprEnd = whileTree.expr.tokens.last.end
        val bodyBegin = whileTree.body.tokens.head.start
        
        def isBeforeExpr(t: Token) = t.start < exprStart
        def isAfterExpr(t: Token)  = t.start >= exprEnd && t.end < bodyBegin

        // get first opening parenthesis
        val leftParen = whileTree.tokens.find(t => isLeftParen(t) && isBeforeExpr(t))

        // get first closing parenthesis after expression
        val rightParen = whileTree.tokens.find(t => isRightParen(t) && isAfterExpr(t))
        
        // add parentheses if necessary
        val addParens = (leftParen, rightParen) match {
          case (Some(_), Some(_)) => Patch.empty
          case (None, None) => Patch.addAround(whileTree.expr, "(", ")")
          case _ => throw new IllegalArgumentException("Invalid syntax: unmatched parentheses")
        }

        // remove DO keyword if necessary
        val doToken = whileTree.tokens.find(isDo)
        
        val removeDo = doToken match {
          case Some(t) => 
            val spaceBeforeDo = whileTree.tokens.find(_.start == t.start - 1).get
            
            Patch.removeTokens(List(spaceBeforeDo, t))
          case None => Patch.empty
        }

        addParens + removeDo

      // Rule 1.3
      case forYieldExpr: Term.ForYield => 
        val enumsStart = forYieldExpr.enums.head.tokens.head.start
        val enumsEnd   = forYieldExpr.enums.last.tokens.last.end
        val bodyBegin  = forYieldExpr.body.children.head.tokens.head.start
        
        val isBeforeEnums = (t: Token) => t.start < enumsStart
        val isAfterEnums  = (t: Token) => t.start >= enumsEnd && t.end < bodyBegin

        // get first opening parenthesis
        val leftParen = forYieldExpr.tokens.find(t => isLeftParen(t) && isBeforeEnums(t))

        // get first closing parenthesis after enums
        val rightParen = forYieldExpr.tokens.find(t => isRightParen(t) && isAfterEnums(t))

        // add parentheses if necessary
        val addParens = (leftParen, rightParen) match {
          case (Some(_), Some(_)) => Patch.empty
          case (None, None) => 
            val addLeftParen  = Patch.addLeft(forYieldExpr.enums.head, "(")
            val addRightParen = Patch.addRight(forYieldExpr.enums.last, ")")
            
            addLeftParen + addRightParen
          case _ => throw new IllegalArgumentException("Invalid syntax: unmatched parentheses")
        }

        addParens

      // Rule 1.4
      case forExpr: Term.For => 
        val enumsStart = forExpr.enums.head.tokens.head.start
        val enumsEnd   = forExpr.enums.last.tokens.last.end
        val bodyBegin  = forExpr.body.children.head.tokens.head.start
        
        def isBeforeEnums(t: Token) = t.start < enumsStart
        def isAfterEnums(t: Token)  = t.start >= enumsEnd && t.end < bodyBegin

        // get first opening parenthesis
        val leftParen = forExpr.tokens.find(t => isLeftParen(t) && isBeforeEnums(t))

        // get first closing parenthesis after enums
        val rightParen = forExpr.tokens.find(t => isRightParen(t) && isAfterEnums(t))

        // add parentheses if necessary
        val addParens = (leftParen, rightParen) match {
          case (Some(_), Some(_)) => Patch.empty
          case (None, None) => 
            val addLeftParen  = Patch.addLeft(forExpr.enums.head, "(")
            val addRightParen = Patch.addRight(forExpr.enums.last, ")")
            
            addLeftParen + addRightParen
          case _ => throw new IllegalArgumentException("Invalid syntax: unmatched parentheses")
        }

        // remove DO keyword if necessary
        val doToken = forExpr.tokens.find(isDo)
        
        val removeDo = doToken match {
          case Some(t) => 
            val spaceBeforeDo = forExpr.tokens.find(_.start == t.start - 1).get
            
            Patch.removeTokens(List(spaceBeforeDo, t))
          case None => Patch.empty
        }

        addParens + removeDo

      // Rule 1.5
      case tryCatchTree: Term.Try if tryCatchTree.catchp.size == 1 && params.useCatchInlining => 
        // tryCatchTree.parent.get.tokens.foreach(x => println(x.text, x.start, x.end, isNewLine(x)))
        val newLine  = tryCatchTree.parent.get.tokens.find(isNewLine).get // go back from tryToken to anything that isn't a space
        val tryToken = tryCatchTree.parent.get.tokens.find(isTry).get
        val indentationSize = tryToken.start - newLine.end
        val matchingIndentation = " " * indentationSize

        val beforeCase = Patch.addLeft(tryCatchTree.cases.head, "{ \n" + matchingIndentation + "  ")
        val atTheEnd   = Patch.addRight(tryCatchTree, "\n" + matchingIndentation + "}")

        beforeCase + atTheEnd
    }.asPatch
  }
}
