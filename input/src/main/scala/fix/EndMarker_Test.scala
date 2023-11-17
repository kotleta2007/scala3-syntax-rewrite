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

  class MyNumber(number: Int):
    println(number)

  case class MyText(text: String):
    println(text)
  
}
