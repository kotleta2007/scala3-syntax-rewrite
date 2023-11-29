package fix
/*
rules = [
  IndentationSyntax
]
IndentationSyntax.addEndMarkers = true
*/
object EndMarker_Test {
  if (true) {
    println(true)
  }

  class MyNumber(number: Int) extends AnyRef {
    println(number)
  }

  case class MyText(text: String):
    println(text)

  extension (m: MyText)
    def myPrint = println(m.text)

}
