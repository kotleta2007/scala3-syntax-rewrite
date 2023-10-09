package fix

import scalafix.v1._
import scala.meta._

class Namedliteralarguments_v1
    extends SemanticRule("Namedliteralarguments_v1") {

  // override def withConfiguration(???)

  override def fix(implicit doc: SemanticDocument): Patch = {
    // println(s"Tree.syntax: " + doc.tree.syntax)
    // println(s"Tree.structure: " + doc.tree.structure)
    // println(s"Tree.structureLabeled: " + doc.tree.structureLabeled)
    
    doc.tree.collect {
      // Rule 1.1
      case ifTree: Term.If =>
        val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
        val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]

        // we shouldn't do this, right?
        val isLeftParenBad = (t: Token) => t.text == "("
        val isRightParenBad = (t: Token) => t.text == ")"
        
        val (condStart, condEnd) = (ifTree.cond.tokens.head.start, ifTree.cond.tokens.last.end)
        println(condStart, condEnd)
        
        val beforeCond = (t: Token) => t.start < condStart
        val afterCond  = (t: Token) => t.start >= condEnd

        // for (token <- ifTree.tokens) println(token, afterCond(token))

        // get first opening parenthesis
        val leftParen = ifTree.tokens.find(t => isLeftParen(t) && beforeCond(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after condition
        val rightParen = ifTree.tokens.find(t => isRightParen(t) && afterCond(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        // add THEN keyword
        // can we use scala.meta.tokens.Token.KwThen ?
        val addThen = Patch.addRight(rightParen, " then")

        // Patch.removeToken()

        /*
        println("start")
        for (token <- ifTree.tokens) println(token, token.start, token.end, isLeftParen(token))
        println("done")
        println("Cond tokens")
        for (token <- ifTree.cond.tokens) println(token, token.pos.start, token.pos.end)
        println(ifTree.cond.tokens.head.start, ifTree.cond.tokens.last.end)
        */
        // ifTree.cond.tokens.tokens.last.end

        // Patch.removeToken(ifTree.cond.tokens.tokens.apply(0))
        /*
        println("start")
        for (token <- ifTree.tokens) println(token)
        println("done")
        */
        removeLeftParen + removeRightParen + addThen
        /*
        val newSyntax = ifTree.elsep match {
          case _: Lit.Unit => s"if ${ifTree.cond} then ${ifTree.thenp}"
          case _: Term => s"if ${ifTree.cond} then ${ifTree.thenp} else ${ifTree.elsep}"
        }
        
        Patch.replaceTree(ifTree, newSyntax)
        */

      // Rule 1.2
      case whileTree: Term.While => 
        val isLeftParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftParen]
        val isRightParen = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightParen]

        val (exprStart, exprEnd) = (whileTree.expr.tokens.head.start, whileTree.expr.tokens.last.end)
        // println(exprStart, exprEnd)
        
        val beforeExpr = (t: Token) => t.start < exprStart
        val afterExpr  = (t: Token) => t.start >= exprEnd

        // get first opening parenthesis
        val leftParen = whileTree.tokens.find(t => isLeftParen(t) && beforeExpr(t)).get
        val removeLeftParen = Patch.removeToken(leftParen)

        // get first closing parenthesis after expression
        val rightParen = whileTree.tokens.find(t => isRightParen(t) && afterExpr(t)).get
        val removeRightParen = Patch.removeToken(rightParen)

        // add DO keyword
        // can we use scala.meta.tokens.Token.KwDo ?
        val addDo = Patch.addRight(rightParen, " do")

        /*
        val newSyntax = s"while ${whileTree.expr} do ${whileTree.body}"
        Patch.replaceTree(whileTree, newSyntax)
        */
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
        /*
        val newEnums = forYieldExpr.enums.map(_.toString()).mkString(" ")
        val newSyntax = s"for ${newEnums} yield ${forYieldExpr.body}"
        Patch.replaceTree(forYieldExpr, newSyntax)
        */
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
        /*
        val newEnums = forExpr.enums.map(_.toString()).mkString(" ")
        val newSyntax = s"for ${newEnums} do ${forExpr.body}"
        Patch.replaceTree(forExpr, newSyntax)
        */
        removeLeftParen + removeRightParen + addDo

      // Rule 1.5
      case tryCatchTree: Term.Try => 
        // for (token <- tryCatchTree.tokens) println(token.text)

        val isLeftBrace  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
        val isRightBrace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

        val isWhitespace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.Whitespace]

        val isCatch = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCatch]
        val isCase  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.KwCase]

        val catchToken = tryCatchTree.tokens.find(t => isCatch(t)).get
        val caseToken  = tryCatchTree.tokens.find(t => isCase(t)).get
        
        // should we add lazy vals when we define the patches?
        // they will be evaluated at the end of each case

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

        // val isEndline = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.AtEOL]
        // val endLine = tryCatchTree.tokens.find(t => isEndline(t) && afterRightBrace(t)).get
        
        val removeWhitespaceBefore = Patch.removeTokens(tryCatchTree.tokens.filter(whitespaceBefore))
        val removeWhitespaceAfter  = Patch.removeTokens(tryCatchTree.tokens.reverse.takeWhile(whitespaceAfter))

        // val removeEndline = Patch.removeToken(endLine)

        val removeLeftBrace = Patch.removeToken(leftBrace)
        val removeRightBrace = Patch.removeToken(rightBrace)

        // remove first curly brace, remove last curly brace, remove indentation before case
        // between catch and }
        /*
        if (tryCatchTree.catchp.size > 1) {
          Patch.empty
        } else {
          val newSyntax = s"try ${tryCatchTree.expr} catch ${tryCatchTree.catchp.head}"
          Patch.replaceTree(tryCatchTree, newSyntax)
        }
        */
        // Patch.empty
        removeWhitespaceBefore + removeWhitespaceAfter + removeLeftBrace + removeRightBrace// + removeLeftBrace + removeRightBrace
      
        
    }.asPatch
  }
}
