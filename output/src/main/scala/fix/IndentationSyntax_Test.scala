package fix

object IndentationSyntax_Test {
  if (true) 
    println("a")
    println("b")
    println("c")

  if (true) 
    println("a")
    println("b")
    println("c")

  if (true) 
    println("a")
    println("b")
    println("c")

  if (true) 
    println("a")
    println("b")
    println("c")
  
  while (true) 
      println("a")
      println("b")
      println("c")

  // spaces instead of tabs
  while (true) 
    println("a")
    println("b")
    println("c")

  val xs = List(1, 2, 3)
  for (x <- xs) 
      println("a")
      println("b")
      println("c")

  for (x <- xs) yield 
    val myFactor = 2
    myFactor * x
}