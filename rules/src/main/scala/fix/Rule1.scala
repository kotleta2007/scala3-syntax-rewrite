package fix

import scalafix.v1._
import scala.meta._
import scala.meta.tokens.Token.LeftParen
import metaconfig.Configured

case class Rule1Parameters(
  useCatchInlining: Boolean
)

object Rule1Parameters {
  val default = Rule1Parameters(true)
  implicit val surface = metaconfig.generic.deriveSurface[Rule1Parameters]
  implicit val decoder = metaconfig.generic.deriveDecoder(Rule1Parameters(true))
}

class Rule1(params: Rule1Parameters)
    extends SemanticRule("Rule1") {
  
  def this() = this(Rule1Parameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    config.conf.getOrElse("Rule1")(this.params).map(newParams => new Rule1(newParams))
  
  override def fix(implicit doc: SemanticDocument): Patch = {
    val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
    val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]
    
    val isLeftBrace  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
    val isRightBrace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

    val isWhitespace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.Whitespace]

    val isCatch = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCatch]
    val isCase  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCase]
    val isThen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwThen]

    doc.tree.collect {
      // Rule 1.1
      case ifTree: Term.If =>
        val (condStart, condEnd) = (ifTree.cond.tokens.head.start, ifTree.cond.tokens.last.end)
        
        val isBeforeCond = (t: Token) => t.start < condStart
        val isAfterCond  = (t: Token) => t.start >= condEnd

        // get first opening parenthesis
        val leftParen = ifTree.tokens.find(t => isLeftParen(t) && isBeforeCond(t)).get
        // ifTree.tokens.collectFirst { case t: LeftParen if t.start >= condEnd => t }
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after condition
        val rightParen = ifTree.tokens.find(t => isRightParen(t) && isAfterCond(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        // add THEN keyword if necessary
        val treeHasThen = ifTree.tokens.exists(isThen)
        val addThen = 
          if (treeHasThen) {
            Patch.addRight(rightParen, " then")
          } else {
            Patch.empty
          }

        removeLeftParen + removeRightParen + addThen

      // Rule 1.2
      case whileTree: Term.While => 
        val (exprStart, exprEnd) = (whileTree.expr.tokens.head.start, whileTree.expr.tokens.last.end)
        
        val isBeforeExpr = (t: Token) => t.start < exprStart
        val isAfterExpr  = (t: Token) => t.start >= exprEnd

        // get first opening parenthesis
        val leftParen = whileTree.tokens.find(t => isLeftParen(t) && isBeforeExpr(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = whileTree.tokens.find(t => isRightParen(t) && isAfterExpr(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        // add DO keyword
        val addDo = Patch.addRight(rightParen, " do")

        removeLeftParen + removeRightParen + addDo

      // Rule 1.3
      case forYieldExpr: Term.ForYield => 
        val (enumsStart, enumsEnd) = (forYieldExpr.enums.head.tokens.head.start, forYieldExpr.enums.last.tokens.last.end)
        
        val isBeforeEnums = (t: Token) => t.start < enumsStart
        val isAfterEnums  = (t: Token) => t.start >= enumsEnd

        // get first opening parenthesis
        val leftParen = forYieldExpr.tokens.find(t => isLeftParen(t) && isBeforeEnums(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = forYieldExpr.tokens.find(t => isRightParen(t) && isAfterEnums(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        removeLeftParen + removeRightParen

      // Rule 1.4
      case forExpr: Term.For => 
        val (enumsStart, enumsEnd) = (forExpr.enums.head.tokens.head.start, forExpr.enums.last.tokens.last.end)
        
        val isBeforeEnums = (t: Token) => t.start < enumsStart
        val isAfterEnums  = (t: Token) => t.start >= enumsEnd

        // get first opening parenthesis
        val leftParen = forExpr.tokens.find(t => isLeftParen(t) && isBeforeEnums(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = forExpr.tokens.find(t => isRightParen(t) && isAfterEnums(t)).get
        val removeRightParen = Patch.removeToken(rightParen)
        
        // add DO keyword
        val addDo = Patch.addRight(rightParen, " do")

        removeLeftParen + removeRightParen + addDo

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
