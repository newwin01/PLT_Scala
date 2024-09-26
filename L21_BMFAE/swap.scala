import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite

def swap(x: Int, y: Int): Unit = {
  var z = y
  val temp = x
  z = temp
}

@main def run(): Unit =
  var a: Int = 10
  var b: Int = 20

  swap(a, b)
  println(a)

