package fix

import scalafix.testkit.AbstractSemanticRuleSuite
import org.scalatest.funsuite.AnyFunSuiteLike
import scalafix.testkit.AbstractSyntacticRuleSuite
import org.scalatest.Args

class RuleSuite extends AbstractSyntacticRuleSuite with AnyFunSuiteLike {
  // runAllTests()
  // checkDiff(Rule1, testNames.head)
  // checkDiff()
  // checkDiff(Rule1, )
  // check(Rule1, "my test", "val x = 2", "val x = 2")

  println()
  val rule = new Rule1()
  check(rule, "My Test", "object main {}", "object main {}")
}