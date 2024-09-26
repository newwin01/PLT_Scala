import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

// Expr definition
trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class App(funExpr: Expr, argExpr: Expr) extends Expr
case class If0(testExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
case class Rec(name: Id, namedExpr: Expr, body: Expr) extends Expr

// DefrdSub definition
trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: Expr_Value, ds: DefrdSub) extends DefrdSub
case class ARecSub(name: Id, var valueBox: Expr_Value, ds: DefrdSub) extends DefrdSub

// Expr_Value definition
trait Expr_Value
case class NumV(n: Int) extends Expr_Value
case class ClosureV(param: Id, body: Expr, ds: DefrdSub) extends Expr_Value
case class ExprV(expr: Expr, ds: DefrdSub, var valueBox: Option[Expr_Value]) extends Expr_Value

class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

def parse(input: String): Expr = {
  object Parser extends RegexParsers {
    def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(s) }
    def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

    lazy val expr: Parser[Expr] =
      int ^^ { case n => Num(n) } |
      symbol |
      wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
      wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
      wrap("*" ~> expr ~ expr) ^^ { case l ~ r => Mul(l, r) } |
      wrap("fun" ~> wrap(symbol) ~ expr) ^^ { case Id(param) ~ body => Fun(Id(param), body) } |
      wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) } |
      wrap("if0" ~> expr ~ expr ~ expr) ^^ { case testExpr ~ thenExpr ~ elseExpr => If0(testExpr, thenExpr, elseExpr) } |
      wrap("rec" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ namedExpr) ~ body => Rec(Id(name), namedExpr, body) }

    def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new Exception(s"bad syntax: $str"))
  }

  Parser.parseAllExpr(input)
}

// strict: Expr_Value -> Expr_Value
def strict(v: Expr_Value): Expr_Value = v match {
  case ev@ExprV(e, ds, box) => box match {
    case Some(cache) => cache
    case _ =>
      val cache = strict(interp(e, ds))
      ev.valueBox = Some(cache)
      cache
  }
  case _ => v
}

// Arithmetic operations
def numOp(op: (Int, Int) => Int)(x: Expr_Value, y: Expr_Value): Expr_Value = (x, y) match {
  case (NumV(x), NumV(y)) => NumV(op(x, y))
}

// passes operation as a lambda function
def numAdd(x: Expr_Value, y: Expr_Value) = numOp(_ + _)(x, y)
def numSub(x: Expr_Value, y: Expr_Value) = numOp(_ - _)(x, y)
def numMul(x: Expr_Value, y: Expr_Value) = numOp(_ * _)(x, y)

// passes operation as a lambda function
// def numAdd = numOp(_ + _)
// def numSub = numOp(_ - _)
// def numMul = numOp(_ * _)

// lookup: symbol DefrdSub -> Expr_Value
def lookup(name: Id, ds: DefrdSub): Expr_Value = ds match {
  case MtSub => throw new Exception(s"Free identifier: $name")
  case ASub(id, v, saved) => if (id == name) v else lookup(name, saved)
  case rec@ARecSub(name, valueBox, ds) =>
    if (rec.name == name) rec.valueBox
    else lookup(name, rec.ds)
}


// numzero? : Expr_Value -> Boolean
def numzero(n: Expr_Value): Boolean = n match {
  case NumV(x) => x == 0
}

// interp: Expr -> Expr_Value
def interp(expr: Expr, ds: DefrdSub): Expr_Value = expr match {
  case Num(n) => NumV(n)
  case Add(l, r) => numAdd(interp(l, ds), interp(r, ds))
  case Sub(l, r) => numSub(interp(l, ds), interp(r, ds))
  case Mul(l, r) => numMul(interp(l, ds), interp(r, ds))
  case Id(s) => strict(lookup(Id(s), ds))
  case Fun(p, b) => ClosureV(p, b, ds)
  case If0(testExpr, thenExpr, elseExpr) =>
    if (numzero(interp(testExpr, ds)))
      interp(thenExpr, ds)
    else
      interp(elseExpr, ds)
  case App(ftn, arg) =>
    val fVal = strict(interp(ftn, ds))
    val aVal = ExprV(arg, ds, None)
    fVal match {
      case ClosureV(param, body, closureDs) => interp(body, ASub(param, aVal, closureDs))
      case _ => throw new Exception("Expected a function")
    }
  case Rec(name, namedExpr, body) =>
    var valueHolder: Expr_Value = NumV(198)
    var newDs = ARecSub(name, valueHolder, ds)
    valueHolder = interp(namedExpr, newDs)
    newDs.valueBox = valueHolder
    interp(body, newDs)
}

// Running the interpreter
def run(sexp: String, ds: DefrdSub): Expr_Value = interp(parse(sexp), ds)

// Test cases
@main def main(): Unit = {
  println(run("{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}", MtSub))
  println(run("{rec {fac {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}} {fac 10}}", MtSub))
}
