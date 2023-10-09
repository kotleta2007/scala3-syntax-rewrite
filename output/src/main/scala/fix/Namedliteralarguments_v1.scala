package fix

object Namedliteralarguments_v1_Test {
  val x = 4

  val comparison = if x == 0 then "equal" else "not equal"
  val oneliner = if x == 1 then println(x)

  val loop = while x >= 0 do { println(x) }
  val ys = List(1, 2, 3)
  val forYield = for y <- ys if y > 0 yield x * x
  
  val forLoop = for y <- ys do { println(y) }

  val tryCatch = 
    try println(x) catch case ex: Exception => println(x)
    
  /*
  val comparison = 
    if (x < 0) {
      "negative"
    } else if (x == 0) {
      "zero"
    } else {
      "positive"
    }
  */
}
