package p1.p2:
/*
rules = [
  AddEndMarkers
]
AddEndMarkers.addEndMarkers = true
*/
  abstract class C():

    def this(x: Int) =
      this()
      if x > 0 then
        val a :: b =
          x :: Nil
        var y =
          x
        while y > 0 do
          println(y)
          y -= 1
        try
          x match
            case 0 => println("0")
            case _ =>
        finally
          println("done")

    def f: String

  object C:
    given C =
      new C:
        def f = "!"

  extension (x: C)
    def ff: String = x.f ++ x.f

end p2
