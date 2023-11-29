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
  end MyText

  extension (m: MyText)
    def myPrint = println(m.text)
    end myPrint
  end extension
end EndMarker_Test