/*
rules = [
  IndentationSyntax
]

IndentationSyntax.addEndMarkers = true
*/
package fix

object IndentationSyntax_Test {
  if (true) {
    println("a")
    println("b")
    println("c")
  }

  if (true) {
    println("a")
      println("b")
          println("c")
  }

  if (true) {
    println("a")
  println("b")
    println("c")
  }

  if (true) 
    println("a")
    println("b")
    println("c")
}
