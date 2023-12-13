/*
rules = [
  IndentationSyntax
]

IndentationSyntax.addEndMarkers = false
*/
// A collection of patterns found when rewriting the community build to indent

trait C1 {

  class CC1
// do not remove braces if empty region
class CC2 {

}
// do not remove braces if open brace is not followed by new line
def m1(x: Int) =
{ x
.toString
  }
// add indent to pass an argument (fewer braces)
def m2: String = {
m1 {
5
}
}
// indent inner method
  def m3: Int = {
def seq = {
Seq(
"1",
"2"
)
}
seq
(1)
.toInt
}
// indent refinement
def m4: Any {
def foo: String
}
=
  new  {
    def foo: String =
    """
Hello, World!
"""
}
// indent end marker
end m4

// fix off-by-one indentation
  //  val x = ""

  // def m5(x: String): String = {
  //     def inner: Boolean = {
  //       true
  //     }
  //      x
  // }

  // unindent properly when needed
  def m6(xs: Seq[String]): String = {
    xs
          .map {
            x => x
          }
        .filter {
          x => x.size > 0
        }
      println("foo")

        def foo: String =
          ""
      foo
  }

// do not remove braces if closing braces not followed by new line
def m7: String = {
val x = "Hi"
x
}; def m8(x: String): String = {
s"""Bye $x ${
  x
}
do not indent in a multiline string"""
}
  def m9 = {
    val foo = ""
    val x = Seq(
      s"${foo}",
      ""
    )
  }

// do not remove braces after closing brace
def m10(x: Int)(y: String) = y * x
m10 { 5 } {
  "foo"
}

  // preserve indent of chained calls
  def m11(xs: Seq[String]) = {
    xs
      .filter {
        _ => true
      }
      xs
      .map { x =>
        val y =
          if (x == "") "empty"
          else x.size.toString
        val z = x + y
        z
      }
      .map { x => xs }.flatMap { xs => xs.map { x =>
        val y =
          if (x == "") "empty"
          else x.size.toString
        val z = x + y
        z
      }}
      .map {
        x => val y =
          if (x == "") "empty"
          else x.size.toString
        val z = x + y
        z
      }
      .map {
        x =>
        val y =
          if (x == "") "empty"
          else x.size.toString
        val z = x + y
        z
      }
  }

  // do not remove braces inside (...) or [...]
  // remove braces after =>
  // def m12(xs: List[Int]) = {
  //   println(
  //     xs.size match {
  //       case 1 =>
  //         xs match {
  //           case 1 :: Nil => "1"
  //           case _ => s"${xs.head} :: Nil"
  //         }
  //       case _ => {
  //         "xs"
  //       }
  //     }
  //   )
  //   println(
  //     if (xs.size > 0) {
  //       "foo"
  //     } else {
  //       "bar"
  //     }
  //   )
  //   xs.map(
  //     x => {
  //       x
  //     }
  //   ).map {
  //     x => {
  //       x
  //     }
  //   }
  // }
  // import reflect.Selectable.reflectiveSelectable
  // def m13(xs: List[
  //   Any {
  //     def foo: String
  //   }
  // ]) =
  //   xs.map(x => x.foo)

  // preserve indentation style before 'case'
  // but fix indentation inside 'case'
  def m14(o: Option[String]) = {
    o match
      case Some(x) => x
      case None => ""

    o match
    case Some(x) => x
    case None => ""
    end match

    o match {
    case None =>
    ""
    case Some(x) =>
    x
    }
  }
  def m15(xs: List[Int]): String = {
    xs match {
      case _ :: tail => {
        if tail.size == 0 then
          println("log")
      }
      "foo"
      case Nil =>
      "bar"
    }
  }

  // add backticks around operator
  object *:{
    def foo = ???
  }
  def m16 =
    val x = 5 * {
      2
    } == 10 || {
      false
    }
    x `&&` {
      true
    } 

  // leading infix operator
  def m17 =
    true
    && {
      false
    }

  // ident ending with '_'
  def m_(x: String) = ???
  m_ {
    "foo"
  }

  // do not remove braces in sequence of blocks
  def m18(using ctx: String) = println(ctx)
  {
    given String = "foo"
    m18
  }
  {
    given String = "bar"
    m18
  }
  def m19(x: String) = {
    {
      given String = "foo"
      m18
    }
    {
      given String = "bar"
      m18
    }
  }

  // back-quote end indent before match
  // def m20 =
  //   val end = "Foo"
  //   end match {
  //     case "Foo" =>
  //     case _ =>
  //   }
  //   end take 3
}

// indent template after self type
class C2 { self =>
val x = ""
}
trait C3 {
  self =>
val x = ""
}
case class C4() {
self =>
  val y = ""
}