import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite

case class Box(var value: Int) {
  def unbox(): Int = value
  def setBox(newValue: Int): Unit = value = newValue
}

def swap(x: Box, y: Box): Unit = {
  val z = Box(y.unbox())
  y.setBox(x.unbox())
  x.setBox(z.unbox())
}

@main def run(): Unit =
  val a = Box(10)
  val b = Box(20)

  swap(a, b)
  println(a.unbox())

