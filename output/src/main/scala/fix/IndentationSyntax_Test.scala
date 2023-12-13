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
  for (x <- xs)
      println("a")
            println("b")
                        println("c")
  end for

  for (x <- xs) yield
    val myFactor = 2
                      myFactor * x
  end for

  object MyObject:
    override def toString(): String = "myObject"

  if (true)
      if (true)
        println("true")
      end if
  end if

  val num = 2
  val numRes = 
    num match
      case 2 => "two"
      case _ => "not two" 
    end match

end IndentationSyntax_Test