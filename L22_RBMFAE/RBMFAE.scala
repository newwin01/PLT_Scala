import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

// Expr definition
trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: Symbol) extends Expr
case class Fun(param: Symbol, body: Expr) extends Expr
case class ReFun(param: Symbol, body: Expr) extends Expr
case class Newbox(v: Expr) extends Expr
case class Setbox(bn: Expr, v: Expr) extends Expr
case class Openbox(v: Expr) extends Expr
case class Seqn(ex1: Expr, ex2: Expr) extends Expr
case class SetVar(id: Symbol, valExpr: Expr) extends Expr
case class App(ftn: Expr, arg: Expr) extends Expr

//;[contract] parse: sexp -> Expr
//;[purprose] to convert s-expression into Expr


def parse(input: String): Expr = {

  //;[contract] parse: sexp -> LFAE
  //;[purprose] to convert s-expression into LFAE
  object Parser extends RegexParsers {
      def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
      def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(Symbol(s)) }
      def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

      lazy val expr: Parser[Expr] =
      int ^^ { case n => Num(n) } |
      symbol |
      wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
      wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
      wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => App(Fun(name, body), value)} |
      wrap("fun" ~> wrap(symbol) ~ expr) ^^ {case Id(param) ~ body => Fun(param, body)} |
      wrap("refun" ~> wrap(symbol) ~ expr) ^^ {case Id(param) ~ body => ReFun(param, body)} |
      wrap("newbox" ~> expr) ^^ { case value => Newbox(value) } |
      wrap("setbox" ~> expr ~ expr) ^^ { case id ~ value => Setbox(id, value) } |
      wrap("openbox" ~> expr) ^^ { case id => Openbox(id) } |
      wrap("seqn" ~> expr ~ expr) ^^ { case ex1 ~ ex2 => Seqn(ex1, ex2) } |
      wrap("setvar" ~> symbol ~ expr) ^^ { case Id(id) ~ ex1 => SetVar(id, ex1) } |
      wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) } 

      def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
  }

  Parser.parseAllExpr(input)

}

trait Expr_Value
case class NumV(n: Int) extends Expr_Value
case class ClosureV(param: Symbol, body: Expr, ds: DefrdSub) extends Expr_Value
case class RefClosV(param: Symbol, body: Expr, ds: DefrdSub) extends Expr_Value
case class BoxV(address: Int) extends Expr_Value

// Arithmetic operations
def numOp(op: (Int, Int) => Int)(x: Expr_Value, y: Expr_Value): Expr_Value = (x, y) match {
  case (NumV(x), NumV(y)) => NumV(op(x, y))
  case (x, y) => throw new SimpleException(s"Not a Number")
}

// passes operation as a lambda function
def numAdd(x: Expr_Value, y: Expr_Value) = numOp(_ + _)(x, y)
def numSub(x: Expr_Value, y: Expr_Value) = numOp(_ - _)(x, y)

trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Symbol, address: Int , saved: DefrdSub) extends DefrdSub

trait Store
case object MtSto extends Store
case class ASto (address: Int, value: Expr_Value, rest: Store) extends Store

// Custom SimpleException class with no stack trace
class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}


def lookup(name: Symbol, ds: DefrdSub): Int = ds match {
  case MtSub => throw new SimpleException(s"Free Identifier: $name")
  case ASub(i, adr, saved) => if (i == name) adr else lookup(name, saved)
}

def storeLookup(address: Int, sto: Store): Expr_Value = sto match {
  case MtSto => throw new SimpleException(s"No Value at Address")
  case ASto(location, value, rest) => if (location == address) value else storeLookup(address, rest)
}


def malloc(st: Store): Int = maxAddress(st) + 1

def maxAddress(st: Store): Int = st match {
  case MtSto => 0
  case ASto(n, _, rest) => Math.max(n, maxAddress(rest))
}

trait ValueStore
case class vs(value: Expr_Value, store: Store) extends ValueStore

//interp: Expr DefrdSub Store -> ValueStore
def interp(expr: Expr, ds: DefrdSub, st: Store): ValueStore = expr match {
  case Num(n) => vs(NumV(n), st)
  case Add(l, r) => interpTwo(l, r, ds, st){ (v1, v2, st1) => vs(numAdd(v1, v2), st)}
  case Sub(l, r) => interpTwo(l, r, ds, st){ (v1, v2, st1) => vs(numSub(v1, v2), st)}
  case Id(s) => vs(storeLookup(lookup(s, ds), st), st)
  case Fun(p, d) => vs(ClosureV(p, d, ds), st)
  case ReFun(p, d) => vs(RefClosV(p, d, ds), st)
  case App(f,a) => interp(f, ds, st) match {
    case vs(f_value, f_store) => f_value match {
      case ClosureV(cParam, cBody, cDs) => interp(a, ds, f_store) match {
        case vs(a_value, a_store) =>
          val newAddress = malloc(a_store)
          f_value match {
            case ClosureV(param, body, ds) =>
              interp(body, ASub(param, newAddress, ds), ASto(newAddress, a_value, a_store))
          }
      }
      case RefClosV(rcParam, rcBody, rcDs) => a match {
        case Id(name) =>
          val address = lookup(name, ds)
          interp(rcBody, ASub(rcParam, address, rcDs), f_store)
      }
    }
  }
  case Newbox(v) => interp(v, ds, st) match {
    case vs(vl, st1) => 
      val a = malloc(st1)
      vs(BoxV(a), ASto(a, vl, st1))
  }
  case Setbox(bx_expr, val_expr) => interpTwo(bx_expr, val_expr, ds, st){ (bx_val, value, st1) => 
    vs(value, ASto( 
      bx_val match { case BoxV(address) => address}, value, st1 )) }
  case Openbox(bx_expr) => interp(bx_expr, ds, st) match {
    case vs(bx_val, st1) => bx_val match {
      case BoxV(address) => vs(storeLookup(address, st1), st1)
    }
  }
  case Seqn(a, b) => interpTwo(a, b, ds, st) { (v1, v2, st1) => vs(v2, st1)}
  case SetVar(id, valExpr) =>
    val a = lookup(id, ds)
    interp(valExpr, ds, st) match {
      case vs(value, st) =>
        vs(value, ASto(a, value, st))
    }
}

def interpTwo(expr1: Expr, expr2: Expr, ds: DefrdSub, st: Store)(handle: (Expr_Value, Expr_Value, Store) => ValueStore): ValueStore = {
  interp(expr1, ds, st) match {
    case vs(val1, st2) =>
      interp(expr2, ds, st2) match {
        case vs(val2, st3) =>
          handle(val1, val2, st3)
      }
  }
}

def run(sexp: String, ds: DefrdSub, st:Store) = interp(parse(sexp), ds, st)

@main def run(): Unit =
  println(run("{with {a 3} {setvar a 5}}", MtSub, MtSto))
  println(run("{with {a 3} {seqn {{refun {x} {setvar x 5}} a} a}}", MtSub, MtSto))
  //println(run("{with {a {newbox 3}} {seqn {{fun {x} {setbox x 5}} a} {openbox a}}}", MtSub, MtSto))
  println(run("{with {swap {refun {x} {refun {y} {with {z x} {seqn {setvar x y} {setvar y z}}}}}} {with {a 10} {with {b 20} {seqn {{swap a} b} a}}}}", MtSub, MtSto))
  println(run("{with {swap {refun {x} {refun {y} {with {z x} {seqn {setvar x y} {setvar y z}}}}}} {with {a 10} {with {b 20} {seqn {{swap a} b} b}}}}", MtSub, MtSto))
  //println(run("{with {swap {fun {x} {fun {y} {with {z x} {seqn {setbox x {openbox y}} {setbox y {openbox z}}}}}}} {with {a {newbox 10}} {with {b {newbox 20}} {seqn {{swap a} b} {openbox a}}}}}", MtSub, MtSto))


def msg = "I was compiled by Scala 3. :)"

