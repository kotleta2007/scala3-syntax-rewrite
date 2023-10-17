/*
rules = [
  Rule1
]

Rule1.useCatchInlining = true
 */
package fix

object Rule1_Test {
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
  val ifMixedSyntax = if (x == 0) then "equal" else "not equal"

  // rule 1.2
  val whileLoop = while (x >= 0) { println(x) }

  // rule 1.3
  val forYield = for (y <- ys if y > 0) yield x * x
  
  // rule 1.4
  val forLoop = for (y <- ys) { println(y) }

  // rule 1.5
  val tryCatch = 
    try println(x) catch { 
      case ex: Exception => println(ex.toString())
    }
  val done = "done" // otherwise, VS Code trims the tabulation on the last line
}
