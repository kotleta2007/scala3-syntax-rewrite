package fix

import scalafix.v1._
import scala.meta._
import scala.meta.tokens.Token.LeftParen
import metaconfig.Configured

case class MyRuleParameters(
  foo: Int,
  bar: Boolean
)

object MyRuleParameters {
  val default = MyRuleParameters(1, true)
  implicit val surface = metaconfig.generic.deriveSurface[MyRuleParameters]
  implicit val decoder = metaconfig.generic.deriveDecoder(MyRuleParameters(1, true))
}

class Namedliteralarguments_v1(params: MyRuleParameters)
    extends SemanticRule("Namedliteralarguments_v1") {
  
  def this() = this(MyRuleParameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    // config.conf.get[MyRuleParameters]("Namedliteralarguments_v1").map(new Namedliteralarguments_v1(_))
    config.conf.getOrElse("Namedliteralarguments_v1")(this.params).map(newParams => new Namedliteralarguments_v1(newParams))
  

  override def fix(implicit doc: SemanticDocument): Patch = {
    val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
    val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]

    doc.tree.collect {
      // Rule 1.1
      case ifTree: Term.If =>
        val (condStart, condEnd) = (ifTree.cond.tokens.head.start, ifTree.cond.tokens.last.end)
        
        val beforeCond = (t: Token) => t.start < condStart
        val afterCond  = (t: Token) => t.start >= condEnd

        // get first opening parenthesis
        val leftParen = ifTree.tokens.find(t => isLeftParen(t) && beforeCond(t)).get
        ifTree.tokens.collectFirst { case t: LeftParen if t.start >= condEnd => t }
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after condition
        val rightParen = ifTree.tokens.find(t => isRightParen(t) && afterCond(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        // add THEN keyword
        // can we use scala.meta.tokens.Token.KwThen ?
        val addThen = Patch.addRight(rightParen, " then")

        removeLeftParen + removeRightParen + addThen

      // Rule 1.2
      case whileTree: Term.While => 
        val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
        val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]

        val (exprStart, exprEnd) = (whileTree.expr.tokens.head.start, whileTree.expr.tokens.last.end)
        
        val beforeExpr = (t: Token) => t.start < exprStart
        val afterExpr  = (t: Token) => t.start >= exprEnd

        // get first opening parenthesis
        val leftParen = whileTree.tokens.find(t => isLeftParen(t) && beforeExpr(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = whileTree.tokens.find(t => isRightParen(t) && afterExpr(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        // add DO keyword
        val addDo = Patch.addRight(rightParen, " do")

        removeLeftParen + removeRightParen + addDo

      // Rule 1.3
      case forYieldExpr: Term.ForYield => 
        val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
        val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]

        val (enumsStart, enumsEnd) = (forYieldExpr.enums.head.tokens.head.start, forYieldExpr.enums.last.tokens.last.end)
        
        val beforeEnums = (t: Token) => t.start < enumsStart
        val afterEnums  = (t: Token) => t.start >= enumsEnd

        // get first opening parenthesis
        val leftParen = forYieldExpr.tokens.find(t => isLeftParen(t) && beforeEnums(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = forYieldExpr.tokens.find(t => isRightParen(t) && afterEnums(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        removeLeftParen + removeRightParen

      // Rule 1.4
      case forExpr: Term.For => 
        val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
        val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]

        val (enumsStart, enumsEnd) = (forExpr.enums.head.tokens.head.start, forExpr.enums.last.tokens.last.end)
        
        val beforeEnums = (t: Token) => t.start < enumsStart
        val afterEnums  = (t: Token) => t.start >= enumsEnd

        // get first opening parenthesis
        val leftParen = forExpr.tokens.find(t => isLeftParen(t) && beforeEnums(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = forExpr.tokens.find(t => isRightParen(t) && afterEnums(t)).get
        val removeRightParen = Patch.removeToken(rightParen)
        
        // add DO keyword
        // can we use scala.meta.tokens.Token.KwDo ?
        val addDo = Patch.addRight(rightParen, " do")

        removeLeftParen + removeRightParen + addDo

      // Rule 1.5
      case tryCatchTree: Term.Try if tryCatchTree.catchp.size == 1 && params.bar => 
        val isLeftBrace  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
        val isRightBrace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

        val isWhitespace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.Whitespace]

        val isCatch = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCatch]
        val isCase  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCase]

        val catchToken = tryCatchTree.tokens.find(t => isCatch(t)).get
        val caseToken  = tryCatchTree.tokens.find(t => isCase(t)).get

        val afterCatch  = (t: Token) => t.start >= catchToken.end
        val afterCase   = (t: Token) => t.start >= caseToken.end
        val beforeCase  = (t: Token) => t.end <= caseToken.start

        val leftBrace  = tryCatchTree.tokens.find(t => isLeftBrace(t) && afterCatch(t)).get
        val rightBrace = tryCatchTree.tokens.find(t => isRightBrace(t) && afterCase(t)).get

        val beforeRightBrace = (t: Token) => t.end <= rightBrace.start
        val afterRightBrace  = (t: Token) => t.start >= rightBrace.end
        val afterLeftBrace  = (t: Token) => t.start >= leftBrace.end

        val whitespaceBefore = (t: Token) => isWhitespace(t) && afterLeftBrace(t) && beforeCase(t)
        val whitespaceAfter  = (t: Token) => isWhitespace(t) && beforeRightBrace(t)
        
        val removeWhitespaceBefore = Patch.removeTokens(tryCatchTree.tokens.filter(whitespaceBefore))
        val removeWhitespaceAfter  = Patch.removeTokens(tryCatchTree.tokens.reverse.takeWhile(whitespaceAfter))

        val removeLeftBrace = Patch.removeToken(leftBrace)
        val removeRightBrace = Patch.removeToken(rightBrace)

        // remove first curly brace, remove last curly brace, remove indentation before case
        // between catch and }
        
        removeWhitespaceBefore + removeWhitespaceAfter + removeLeftBrace + removeRightBrace
    }.asPatch
  }
}
