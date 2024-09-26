import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

// LFAE definition
trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class App(funExpr: Expr, argExpr: Expr) extends Expr

trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: Expr_Value, saved: DefrdSub) extends DefrdSub

trait Expr_Value
case class NumV(n: Int) extends Expr_Value
case class ClosureV(param: Id, value: Expr, ds: DefrdSub) extends Expr_Value
case class ExprV(expr: Expr, ds: DefrdSub, var v: Option[Expr_Value]) extends Expr_Value

class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

// strict: RCFAE_Value -> RCFAE_Value
def strict(v: Expr_Value): Expr_Value = v match {
  case ev@ExprV(e, ds, box) => box match {
      case Some(cache) => cache
      case _ =>
        val cache = strict(interp(e, ds))
        ev.v = Some(cache)
        cache
    }
  case _ => v
}

def parse(input: String): Expr = {

    //;[contract] parse: sexp -> LFAE
  //;[purprose] to convert s-expression into LFAE
  object Parser extends RegexParsers {
    def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(s) }
    def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

    lazy val expr: Parser[Expr] =
      int ^^ { case n => Num(n) } |
      symbol |
      wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
      wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
      wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => App(Fun(Id(name), body), value)} |
      wrap("fun" ~> wrap(symbol) ~ expr) ^^ {case Id(param) ~ body => Fun(Id(param), body)} |
      wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) } 

    def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
  }

  Parser.parseAllExpr(input)

}

// Arithmetic operations
def numOp(op: (Int, Int) => Int)(x: Expr_Value, y: Expr_Value): Expr_Value = (strict(x), strict(y)) match {
  case (NumV(x), NumV(y)) => NumV(op(x, y))
  case (x, y) => throw new SimpleException(s"Not a Number")
}

// passes operation as a lambda function
def numAdd(x: Expr_Value, y: Expr_Value) = numOp(_ + _)(x, y)
def numSub(x: Expr_Value, y: Expr_Value) = numOp(_ - _)(x, y)
def numMul(x: Expr_Value, y: Expr_Value) = numOp(_ * _)(x, y)

//might not works for certain version
// def numAdd = numOp(_ + _)
// def numSub = numOp(_ - _)
// def numMul = numOp(_ * _)


def lookup(name: Id, ds: DefrdSub): Expr_Value = ds match {
  case MtSub => throw new SimpleException(s"Free Identifier: $name")
  case ASub(i, v, saved) => if (i == name) strict(v) else lookup(name, saved)
}


//;interp: f1wae list-of-FuncDef -> number
def interp(expr: Expr, ds: DefrdSub): Expr_Value = expr match {
  case Num(n) => NumV(n)
  case Add(l, r) => numAdd(interp(l, ds), interp(r, ds))
  case Sub(l, r) => numSub(interp(l, ds), interp(r, ds))
  // case With(i, v, e) => interp(subst(e, i, (interp (v))))
  case Id(s) => lookup(Id(s), ds)
  case Fun(p, b) => ClosureV(p, b, ds)
  case App(ftn, arg) => 
    val fVal = strict(interp(ftn, ds))
    val aVal = ExprV(arg, ds, None)
    fVal match {
      case ClosureV(param, body, ds) => interp(body, ASub(param, aVal, ds))
      case _ => throw new SimpleException("Expected a function")
    }
}

def run(sexp: String, ds: DefrdSub) = interp(parse(sexp), ds)

@main def hello(): Unit =
  println("Hello World main")
  println(parse("{+ 1 2}"))
  println(parse("{{fun {x} {+ x x}} 10}"))
  println(parse("{{fun {x} {+ x 1}} 10}"))
  assert(parse("{fun {x} {+ x 1}}") == Fun(Id("x"),Add(Id("x"),Num(1))))
  assert(parse("{{fun {x} {+ x x}} 10}") == App(Fun(Id("x"),Add(Id("x"),Id("x"))),Num(10)))
  assert(parse("{fun {x} {+ x 1}}") == Fun(Id("x"),Add(Id("x"),Num(1))))
  println(parse("{{fun {x} {+ 1 x}} 10}"))
  println(parse("{with {x 3} {+ x x}}"))
  println(parse("{with {x 3} {+ x x}}"))
  println(parse("{with {x {+ 5 5}} {+ x x}}"))


  println(interp(parse("{fun {y} {+ x y}}"), MtSub))
  println(interp(parse("{{fun {x} {+ 1 x}} 10}"), MtSub))
  println(interp(parse("{with {x 3} {fun {x} {+ x y}}}"), MtSub))
  println(interp(parse("{with {x 5} x}"), MtSub))
  println(interp(parse("{with {x {+ 5 5}} {+ x x}}"), MtSub))
  println(interp(parse("{with {x 5} {+ x {with {y 5} y} } }"),  MtSub))

  println(interp(parse("{fun {y} {+ x y}}"), MtSub)) // (run '{fun {y} {+ x y}} (mtSub))
  println(interp(parse("{{fun {x} {+ 1 x}} 10}"), MtSub)) // (run '{{fun {x} {+ 1 x}} 10} (mtSub))
  println(interp(parse("{{fun {x} 0} {+ 1 {fun {y} 2}}}"), MtSub)) // (run '{{fun {x} 0} {+ 1 {fun {y} 2}}} (mtSub))
  println(interp(parse("{+ 1 {{fun {y} 2} 2}}"), MtSub)) // Not a Number
  // println(interp(parse("{{fun {x} x} {+ 1 {fun {y} 2}}}"))) // (run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub))
  // println(interp(parse("{{fun {x} x} {+ 1 {{fun {y} 2} 1}}}"))) // (run '{{fun {x} x} {+ 1 {{fun {y} 2} 1}}} (mtSub))
  println(interp(parse("{{fun {x} x} {+ 1 1}}"), MtSub)) // (run '{{fun {x} x} {+ 1 1}} (mtSub))

  println(interp(parse("{{fun {x} x} 1}"), MtSub)) // (run '{{fun {x} x} 1} (mtSub))
  println(interp(parse("{{fun {x} {+ x x}} 1}"), MtSub)) // (run '{{fun {x} {+ x x}} 1} (mtSub))
  // println(interp(parse("{{fun {x} x} {+ 1 {fun {y} 2}}}"))) // (run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub))
  // println(interp(parse("{{fun {x} {+ x x}} {+ 1 {fun {y} 2}}}"))) // (run '{{fun {x} {+ x x}} {+ 1 {fun {y} 2}}} (mtSub))
  // println(interp(parse("{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}}"))) // (run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
  println(interp(parse("{{fun {x} {+ x x}} 1}"), MtSub)) // (run '{{fun {x} {+ x x}} 1} (mtSub))
  println(interp(parse("{{fun {x} {+ {+ x x} {+ x x}}} {- {+ 4 5} {+ 8 9}}}"), MtSub)) // (run '{{fun {x} {+ {+ x x} {+ x x}}} {- {+ 4 5} {+ 8 9}}} (mtSub))

  println(interp(parse("{{fun {x} {x {+ 4 5}}} {fun {x} 0}}"), MtSub)) // (run '{{fun {x} {x {+ 4 5}}} {fun {x} 0}} (mtSub))
  // println(interp(parse("{{fun {x} 0} {+ 1 {fun {y} 2}}}"))) // (run '{{fun {x} 0} {+ 1 {fun {y} 2}}} (mtSub))
  // println(interp(parse("{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}}"))) // (run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
  println(interp(parse("{{fun {x} x} 1}"), MtSub)) // (run '{{fun {x} x} 1} (mtSub))
  // println(interp(parse("{{fun {x} x} {+ 1 {{fun {y} 2} 1}}}"))) // (run '{{fun {x} x} {+ 1 {{fun {y} 2} 1}}} (mtSub))

  println(interp(parse("{{fun {f} {f 1}} {fun {x} {+ x 1}}}"), MtSub)) // (run '{{fun {f} {f 1}} {fun {x} {+ x 1}}} (mtSub))
  println(interp(parse("{with {k {fun{x} x}} {{fun {f} {f 1}} k}}"), MtSub)) // (run '{with {k {fun{x} x}} {{fun {f} {f 1}} k}} (mtSub))
  println(interp(parse("{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}}"), MtSub)) // (run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
  println(interp(parse("{with {a {fun {f} {+ f f}}} {a 3}}"), MtSub)) // (run '{with {a {fun {f} {+ f f}}} {a 3}} (mtSub))
  println(parse("{with {a {fun {f} {+ f f}}} {a 3}}"))

  println(run("{with {k {fun{x} x}} {{fun {f} {f 1}} k}}", MtSub))


   println(msg)

def msg = "I was compiled by Scala 3. :)"
