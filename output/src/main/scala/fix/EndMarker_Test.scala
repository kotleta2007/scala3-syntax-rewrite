package fix

object EndMarker_Test:
  if (true) 
    println(true)
  end if

  class MyNumber(number: Int):
    println(number)
  end MyNumber

  case class MyText(text: String):
    println(text)
  end MyText
  

end EndMarker_Test