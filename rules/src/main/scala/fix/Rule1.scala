package fix

import scalafix.v1._
import scala.meta._
import scala.meta.tokens.Token.LeftParen
import metaconfig.Configured

case class Rule1Parameters(
  useCatchInlining: Boolean
)

object Rule1Parameters {
  val default = Rule1Parameters(false)
  implicit val surface: metaconfig.generic.Surface[Rule1Parameters] = metaconfig.generic.deriveSurface[Rule1Parameters]
  implicit val decoder: metaconfig.ConfDecoder[Rule1Parameters] = metaconfig.generic.deriveDecoder(default)
}

class Rule1(params: Rule1Parameters)
    extends SemanticRule("Rule1") {
  
  def this() = this(Rule1Parameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    config.conf.getOrElse("Rule1")(this.params).map(newParams => new Rule1(newParams))
  
  override def fix(implicit doc: SemanticDocument): Patch = {
    val isLeftParen  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
    val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]
    
    val isLeftBrace  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
    val isRightBrace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

    val isWhitespace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.Whitespace]

    val isCatch = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCatch]
    val isCase  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCase]
    val isThen  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwThen]
    val isDo    = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwDo]

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
        // ifTree.tokens.collectFirst { case t: LeftParen if t.start >= condEnd => t }
        val removeLeftParen = leftParen.map(Patch.removeToken)

        // get first closing parenthesis after condition
        val rightParen = ifTree.tokens.find(t => isRightParen(t) && isAfterCond(t))
        val removeRightParen = rightParen.map(Patch.removeToken)

        // add THEN keyword if necessary
        val treeHasThen = ifTree.tokens.exists(isThen)
        val addThen = Option.when(!treeHasThen)(Patch.addRight(ifTree.cond, " then"))

        Patch.empty + removeLeftParen + removeRightParen + addThen

      // Rule 1.2
      case whileTree: Term.While => 
        val exprStart = whileTree.expr.tokens.head.start
        val exprEnd = whileTree.expr.tokens.last.end
        val bodyBegin = whileTree.body.tokens.head.start
        
        def isBeforeExpr(t: Token) = t.start < exprStart
        def isAfterExpr(t: Token)  = t.start >= exprEnd && t.end < bodyBegin

        // get first opening parenthesis
        val leftParen = whileTree.tokens.find(t => isLeftParen(t) && isBeforeExpr(t))
        val removeLeftParen = leftParen.map(Patch.removeToken)

        // get first closing parenthesis after expression
        val rightParen = whileTree.tokens.find(t => isRightParen(t) && isAfterExpr(t))
        val removeRightParen = rightParen.map(Patch.removeToken)

        // add DO keyword if necessary
        val treeHasDo = whileTree.tokens.exists(isDo)
        val addDo = Option.when(!treeHasDo)(Patch.addRight(whileTree.expr, " do"))

        Patch.empty + removeLeftParen + removeRightParen + addDo

      // Rule 1.3
      case forYieldExpr: Term.ForYield => 
        val enumsStart = forYieldExpr.enums.head.tokens.head.start
        val enumsEnd   = forYieldExpr.enums.last.tokens.last.end
        val bodyBegin  = forYieldExpr.body.children.head.tokens.head.start
        
        val isBeforeEnums = (t: Token) => t.start < enumsStart
        val isAfterEnums  = (t: Token) => t.start >= enumsEnd && t.end < bodyBegin

        // get first opening parenthesis
        val leftParen = forYieldExpr.tokens.find(t => isLeftParen(t) && isBeforeEnums(t))
        val removeLeftParen = leftParen.map(Patch.removeToken)

        // get first closing parenthesis after expression
        val rightParen = forYieldExpr.tokens.find(t => isRightParen(t) && isAfterEnums(t))
        val removeRightParen = rightParen.map(Patch.removeToken)

        Patch.empty + removeLeftParen + removeRightParen

      // Rule 1.4
      case forExpr: Term.For => 
        val enumsStart = forExpr.enums.head.tokens.head.start
        val enumsEnd   = forExpr.enums.last.tokens.last.end
        val bodyBegin  = forExpr.body.children.head.tokens.head.start
        
        def isBeforeEnums(t: Token) = t.start < enumsStart
        def isAfterEnums(t: Token)  = t.start >= enumsEnd && t.end < bodyBegin

        // get first opening parenthesis
        val leftParen = forExpr.tokens.find(t => isLeftParen(t) && isBeforeEnums(t))
        val removeLeftParen = leftParen.map(Patch.removeToken)

        // get first closing parenthesis after expression
        val rightParen = forExpr.tokens.find(t => isRightParen(t) && isAfterEnums(t))
        val removeRightParen = rightParen.map(Patch.removeToken)
        
        // add DO keyword (if necessary)
        val treeHasDo = forExpr.tokens.exists(isDo)
        val addDo = Option.when(!treeHasDo)(Patch.addRight(forExpr.enums.last, " do"))

        Patch.empty + removeLeftParen + removeRightParen + addDo

      // Rule 1.5
      case tryCatchTree: Term.Try if tryCatchTree.catchp.size == 1 && params.useCatchInlining => 
        val catchToken = tryCatchTree.tokens.find(t => isCatch(t)).get
        val caseToken  = tryCatchTree.tokens.find(t => isCase(t)).get

        val isAfterCatch  = (t: Token) => t.start >= catchToken.end
        val isAfterCase   = (t: Token) => t.start >= caseToken.end
        val isBeforeCase  = (t: Token) => t.end <= caseToken.start

        val leftBrace  = tryCatchTree.tokens.find(t => isLeftBrace(t) && isAfterCatch(t)).get
        val rightBrace = tryCatchTree.tokens.find(t => isRightBrace(t) && isAfterCase(t)).get

        val isBeforeRightBrace = (t: Token) => t.end <= rightBrace.start
        val isAfterRightBrace  = (t: Token) => t.start >= rightBrace.end
        val isAfterLeftBrace  = (t: Token) => t.start >= leftBrace.end

        val whitespaceBefore = (t: Token) => isWhitespace(t) && isAfterLeftBrace(t) && isBeforeCase(t)
        val whitespaceAfter  = (t: Token) => isWhitespace(t) && isBeforeRightBrace(t)
        
        // remove whitespace (indentation) between { and CASE
        // remove whitespace (indentation) before }
        val removeWhitespaceBefore = Patch.removeTokens(tryCatchTree.tokens.filter(whitespaceBefore))
        val removeWhitespaceAfter  = Patch.removeTokens(tryCatchTree.tokens.reverse.takeWhile(whitespaceAfter))

        // remove the braces
        val removeLeftBrace = Patch.removeToken(leftBrace)
        val removeRightBrace = Patch.removeToken(rightBrace)

        removeWhitespaceBefore + removeWhitespaceAfter + removeLeftBrace + removeRightBrace
    }.asPatch
  }
}
