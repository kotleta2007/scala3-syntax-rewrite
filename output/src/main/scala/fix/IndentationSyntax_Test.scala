package fix

object IndentationSyntax_Test:
  if (true) 
    println("a")
    println("b")
    println("c")
  end if

  if (true) 
    println("a2")
    println("b2")
    println("c2")
  end if

  if (true) 
    println("a")
    println("b")
    println("c")
  end if

  if (true) 
    println("a")
    println("b")
    println("c")
  end if
  
  while (true) 
      println("a")
      println("b")
      println("c")
  end while

  // spaces instead of tabs
  while (true) 
    println("a")
    println("b")
    println("c")
  end while

  val xs = List(1, 2, 3)
  end xs
  for (x <- xs) 
      println("a")
      println("b")
      println("c")

  for (x <- xs) yield 
    val myFactor = 2
    end myFactor
    myFactor * x

  object MyObject:
    override def toString(): String = "myObject"
    end toString
  end MyObject

end IndentationSyntax_Test