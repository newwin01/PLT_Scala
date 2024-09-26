import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

// Expr definition
trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class Newbox(v: Expr) extends Expr
case class Setbox(bn: Expr, v: Expr) extends Expr
case class Openbox(v: Expr) extends Expr
case class Seqn(ex1: Expr, ex2: Expr) extends Expr
case class App(ftn: Expr, arg: Expr) extends Expr

//;[contract] parse: sexp -> Expr
//;[purprose] to convert s-expression into Expr


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
      wrap("newbox" ~> expr) ^^ { case value => Newbox(value) } |
      wrap("setbox" ~> expr ~ expr) ^^ { case id ~ value => Setbox(id, value) } |
      wrap("openbox" ~> expr) ^^ { case id => Openbox(id) } |
      wrap("seqn" ~> expr ~ expr) ^^ { case ex1 ~ ex2 => Seqn(ex1, ex2) } |
      wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) } 

      def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
  }

  Parser.parseAllExpr(input)

}

trait Expr_Value
case class NumV(n: Int) extends Expr_Value
case class ClosureV(param: Id, body: Expr, ds: DefrdSub) extends Expr_Value
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
case class ASub(name: Id, address: Int , saved: DefrdSub) extends DefrdSub

trait Store
case object MtSto extends Store
case class ASto (address: Int, value: Expr_Value, rest: Store) extends Store

// Custom SimpleException class with no stack trace
class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}


def lookup(name: Id, ds: DefrdSub): Int = ds match {
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

//;interp: f1wae list-of-FuncDef -> number
def interp(expr: Expr, ds: DefrdSub, st: Store): ValueStore = expr match {
  case Num(n) => vs(NumV(n), st)
  case Add(l, r) => interp(l, ds, st) match {
    case vs(l_value, l_store) => interp(r, ds, l_store) match {
      case vs(r_value, r_store) => vs(numAdd(l_value, r_value), r_store)
    }
  }
  case Sub(l, r) => interp(l, ds, st) match {
    case vs(l_value, l_store) => interp(r, ds, l_store) match {
      case vs(r_value, r_store) => vs(numSub(l_value, r_value), r_store)
    }
  }
  case Id(s) => vs(storeLookup(lookup(Id(s), ds), st), st)
  case Fun(p,d) => vs(ClosureV(p, d, ds), st)
  case App(f,a) => interp(f, ds, st) match {
    case vs(f_value, f_store) => interp(a, ds, f_store) match {
      case vs(a_value, a_store) => 
        val newAddress = malloc(a_store)
        f_value match {
          case ClosureV(param, body, ds) => 
            interp(body, ASub(param, newAddress, ds), ASto(newAddress, a_value, a_store))
        }
    }
  }
  case Newbox(v) => interp(v, ds, st) match {
    case vs(vl, st1) => 
      val a = malloc(st1)
      vs(BoxV(a), ASto(a, vl, st1))
  }
  case Setbox(bx_expr, val_expr) => interp(bx_expr, ds, st) match {
    case vs(bx_val, st2) => interp(val_expr, ds, st2) match {
      case vs(value, st3) => bx_val match {
        case BoxV(address) => vs(value, ASto(address, value, st3))
      }
    }
  }
  case Openbox(bx_expr) => interp(bx_expr, ds, st) match {
    case vs(bx_val, st1) => bx_val match {
      case BoxV(address) => vs(storeLookup(address, st1), st1)
    }
  }
  case Seqn(a, b) => interp(a, ds, st) match {
    case vs(a_value, a_store) => interp(b, ds, a_store)
  }
    
}

def run(sexp: String, ds: DefrdSub, st:Store) = interp(parse(sexp), ds, st)

@main def run(): Unit =

  println(run("{newbox 7}", MtSub, MtSto))

  println(run("7", MtSub, MtSto))
  println(run("{+ 2 3}", MtSub, MtSto))

  // println(run("{setbox b 7}", MtSub, MtSto))

  println(run("{with {b {newbox 10}} {seqn {setbox b 7} {openbox b}}}", MtSub, MtSto))

  println(run("{with {b {newbox 10}} {seqn {{fun {c} {setbox c 7}} b} {openbox b}}}", MtSub, MtSto))

  println(run("{+ {with {b {newbox 10}} {seqn {setbox b 7} {openbox b}}} {with {b {newbox 10}} {seqn {setbox b 5} {openbox b}}}}", MtSub, MtSto))

  // println(run("{+ {with {b {newbox 3}} {seqn {setbox b 10} {openbox b}}} 2}", MtSub, MtSto))
  // println(run("{newbox 7}", MtSub, MtSto))
  // println(run("{with {b {newbox 7}} b}", MtSub, MtSto))

  // println(run("{with {a 7} a}", MtSub, MtSto))
  // println(run("{{fun {b} {seqn {setbox b 10} {openbox b}}} {newbox 7}}", MtSub, MtSto))
  // println(run("{with {b {newbox 4}} {+ {openbox b} {seqn {setbox b 10} {openbox b}}}}", MtSub, MtSto))

  // println(run("{newbox {+ 2 3}}", MtSub, MtSto))

  // println(run("{with {b {newbox {+ 2 3}}} {openbox b}}", MtSub, MtSto))

  // Example in slides
  // println(run("{with {b {newbox 7}} {seqn {setbox b 10} {openbox b}}}", MtSub, MtSto))
  // println(run("{{fun {b} {seqn {setbox b 10} {openbox b}}} {newbox 7}}", MtSub, MtSto))

  println(run("{{fun {b} {seqn {seqn {seqn {setbox b 111} {openbox b}} {setbox b 222}} {seqn {setbox b 333} {seqn {setbox b 444} {openbox b}}}}} {newbox 7}}", MtSub, MtSto))

  println(run("{with {b {newbox 1}} {+ {with {b {newbox 10}} {seqn {setbox b 7} {openbox b}}} {with {a {newbox 10}} {seqn {seqn {setbox a 5} {setbox a 1}} {+ 4 {openbox a}}}}}}", MtSub, MtSto))

  println(run("{with {b {newbox 10}} {+ {+ {seqn {setbox b 7} {openbox b}} {seqn {setbox b 10} {openbox b}}} {openbox b}}}", MtSub, MtSto))

  println(run("{with {b {newbox 7}} {seqn {setbox b 10} {openbox b}}}", MtSub, MtSto))

  println(run("{with {b {newbox 7}} {with {c {setbox b 10}} {openbox b}}}", MtSub, MtSto))

  println(run("{with {b {newbox 7}} b}", MtSub, MtSto))

  // println(run("{with {b {newbox 7}} {seqn {with {b {newbox 10}} {setbox b 11}} {openbox b}}}", MtSub, MtSto))

  // println(run("{with {b {newbox 7}} {openbox b}}", MtSub, MtSto))

  // println(run("{with {b {newbox 7}} {setbox b 10}}", MtSub, MtSto))

  // println(run("{with {b {newbox 7}} {seqn {openbox b} {openbox b}}}", MtSub, MtSto))

  println(run("{with {a {newbox 7}} {+ {seqn {setbox a 7} {openbox a}} {openbox a}}}", MtSub, MtSto))

  println(run("{with {b {newbox 7}} {seqn {setbox b 10} b}}", MtSub, MtSto))
  println(run("{with {b {newbox 7}} {seqn {setbox b 10} {openbox b}}}", MtSub, MtSto))


  def msg = "I was compiled by Scala 3. :)"

