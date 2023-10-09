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
        val newEnums = forYieldExpr.enums.map(_.toString()).mkString(" ")
        val newSyntax = s"for ${newEnums} yield ${forYieldExpr.body}"
        Patch.replaceTree(forYieldExpr, newSyntax)

      // Rule 1.4
      case forExpr: Term.For => 
        val newEnums = forExpr.enums.map(_.toString()).mkString(" ")
        val newSyntax = s"for ${newEnums} do ${forExpr.body}"
        Patch.replaceTree(forExpr, newSyntax)

      // Rule 1.5
      case tryCatchTree: Term.Try => 
        if (tryCatchTree.catchp.size > 1) {
          Patch.empty
        } else {
          val newSyntax = s"try ${tryCatchTree.expr} catch ${tryCatchTree.catchp.head}"
          Patch.replaceTree(tryCatchTree, newSyntax)
        }
        
    }.asPatch
  }
}
