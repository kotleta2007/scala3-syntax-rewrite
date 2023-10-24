package fix

object Scala2ControlSyntax_Test {
  val x = 4
  val ys = List(1, 2, 3)

  // rule 1.1
  val ifThenElse = if (x == 0) "equal" else "not equal"
  val ifThen = if (x == 1) println(x)
  val ifElseIf = 
    if (x < 0) 
      "negative"
    else if (x == 0) 
      "zero"
    else 
      "positive"
  val ifMixedSyntax = if (x == 0) "equal" else "not equal"
  val ifNewSyntax = if (x == 0) "equal" else "not equal"
  val ifParensInsideCond = if ((x > 0) && (x < 0)) "not possible" else "not possible"

  // rule 1.2
}
