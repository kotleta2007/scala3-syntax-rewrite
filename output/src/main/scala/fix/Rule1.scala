package fix

object Rule1_Test {
  val x = 4
  val ys = List(1, 2, 3)

  // rule 1.1
  val ifThenElse = if x == 0 then "equal" else "not equal"
  val ifThen = if x == 1 then println(x)
  val ifElseIf = 
    if x < 0 then 
      "negative"
    else if x == 0 then 
      "zero"
    else 
      "positive"
  val ifMixedSyntax = if x == 0 then "equal" else "not equal"
  val ifNewSyntax = if x == 0 then "equal" else "not equal"
  val ifParensInsideCond = if (x > 0) && (x < 0) then "not possible" else "not possible"

  // rule 1.2
  val whileOldSyntax   = while x >= 0 do { println(x) }
  val whileMixedSyntax = while x >= 0 do { println(x) }
  val whileNewSyntax   = while x >= 0 do { println(x) }

  // rule 1.3
  val forYieldOld = for y <- ys if y > 0 yield x * x
  val forYieldNew = for y <- ys if y > 0 yield x * x
  
  // rule 1.4
  val forLoopOld   = for y <- ys do { println(y) }
  val forLoopNew   = for y <- ys do { println(y) }
  val forLoopMixed = for y <- ys do { println(y) }

  // rule 1.5
  val tryCatchOne = 
    try println(x) catch case ex: Exception => println(ex.toString())
    
  val tryCatchTwo = 
    try println(x) catch { 
      case ex: Exception => println(ex.toString())
      case ex: NullPointerException => println("Null pointer exception")
    }
}
