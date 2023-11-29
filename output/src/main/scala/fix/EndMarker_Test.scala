package fix

object EndMarker_Test:
  if (true)
    println(true)
  end if

  class MyNumber(number: Int) extends AnyRef:
    println(number)
  end MyNumber

  case class MyText(text: String):
    println(text)

  extension (m: MyText)
    def myPrint = println(m.text)

end EndMarker_Test