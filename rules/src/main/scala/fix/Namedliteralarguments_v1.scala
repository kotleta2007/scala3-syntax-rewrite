package fix

import scalafix.v1._
import scala.meta._

class Namedliteralarguments_v1
    extends SemanticRule("Namedliteralarguments_v1") {

  // override def withConfiguration(???)

  override def fix(implicit doc: SemanticDocument): Patch = {
    println(s"Tree.syntax: " + doc.tree.syntax)
    println(s"Tree.structure: " + doc.tree.structure)
    println(s"Tree.structureLabeled: " + doc.tree.structureLabeled)
    
    doc.tree.collect {
      // case t @ q"true" => Patch.addLeft(t, "true")
      // case t @ q"if ($cond) $consequent else $alternative" => Patch.replaceSymbols((consequent.toString(), consequent.toString()))
      // case t @ q"if ($ifClause) $thenClause else $elseClause" => Patch.empty
      case ifTree: Term.If =>
        
        val newSyntax = ifTree.elsep match {
          case _: Lit.Unit => s"if ${ifTree.cond} then ${ifTree.thenp}"
          case _: Term => s"if ${ifTree.cond} then ${ifTree.thenp} else ${ifTree.elsep}"
        }

        Patch.replaceTree(ifTree, newSyntax)
      case whileTree: Term.While => 
        val newSyntax = s"while ${whileTree.expr} do ${whileTree.body}"
        Patch.replaceTree(whileTree, newSyntax)
      // if followed by yield, remove parens
      case forYieldExpr: Term.ForYield => 
        val newEnums = forYieldExpr.enums.map(_.toString()).mkString(" ")
        val newSyntax = s"for ${newEnums} yield ${forYieldExpr.body}"
        Patch.replaceTree(forYieldExpr, newSyntax)
      // 
      case forExpr: Term.For => 
        val newEnums = forExpr.enums.map(_.toString()).mkString(" ")
        val newSyntax = s"for ${newEnums} do ${forExpr.body}"
        Patch.replaceTree(forExpr, newSyntax)

      case tryCatchTree: Term.Try => 
        if (tryCatchTree.catchp.size > 1) {
          Patch.empty
        } else {
          val newSyntax = s"try ${tryCatchTree.expr} catch ${tryCatchTree.catchp.head}"
          Patch.replaceTree(tryCatchTree, newSyntax)
        }
          
      // case ifTree @ Term.If(cond, thenp, elsep) => Patch.empty
    }.asPatch
  }
}
