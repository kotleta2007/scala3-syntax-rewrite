/*
rule = Rule1
 */
package fix

object Rule1_Test {
  val x = 4

  val comparison = if (x == 0) "equal" else "not equal"
  val oneliner = if (x == 1) println(x)

  val loop = while (x >= 0) { println(x) }
  val ys = List(1, 2, 3)
  val forYield = for (y <- ys if y > 0) yield x * x
  
  val forLoop = for (y <- ys) { println(y) }

  val tryCatch = 
    try println(x) catch { 
      case ex: Exception => println(x)
    }
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
