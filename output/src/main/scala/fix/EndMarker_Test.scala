package fix

object EndMarker_Test:
  if true then
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
  
  enum Color:
    case Red, Green, Blue
  end Color
end EndMarker_Test

package object A
end A