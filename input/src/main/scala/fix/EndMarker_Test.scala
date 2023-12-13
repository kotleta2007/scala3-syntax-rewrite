package fix
/*
rules = [
  AddEndMarkers
]
AddEndMarkers.addEndMarkers = true
*/
object EndMarker_Test:
  if true then
    println(true)

  class MyNumber(number: Int) extends AnyRef:
    println(number)

  case class MyText(text: String):
    println(text)

  extension (m: MyText)
    def myPrint = println(m.text)
  
  enum Color:
    case Red, Green, Blue

package object A
