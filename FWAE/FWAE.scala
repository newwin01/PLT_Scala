import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class With(name: Id, nameExp: Expr, body: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class App(ftn: Expr, arg: Expr) extends Expr


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
      wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => With(Id(name), value, body) } |
      wrap("fun" ~> wrap(symbol) ~ expr) ^^ {case Id(param) ~ body => Fun(Id(param), body)} |
      wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) }

    def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
  }

  Parser.parseAllExpr(input)
}

def subst(expr: Expr, idtf: Id, value: Expr): Expr = expr match {
  case Num(num) => expr
  case Add(lhs, rhs) => Add(subst(lhs, idtf, value), subst(rhs, idtf, value))
  case Sub(lhs, rhs) => Sub(subst(lhs, idtf, value), subst(rhs, idtf, value))
  case With(i, v, e) =>
    With(i, subst(v, idtf, value), if (i == idtf) e else subst(e, idtf, value))
  case Id(s) => if (Id(s) == idtf) value else expr
  case App(ftn, arg) => App(subst(ftn, idtf, value), subst(arg, idtf, value))
  case Fun(id, body) => if(id == idtf) expr else Fun(id, subst(body, idtf, value))
}

def numAdd(x: Expr, y: Expr): Expr = (x, y) match {
  case (Num(x), Num(y)) => Num(x + y)
}

def numSub(x: Expr, y: Expr): Expr = (x, y) match {
  case (Num(x), Num(y)) => Num(x - y)
}

def numOperator(x: Expr, y: Expr, operator: String): Expr = (x, y) match {
  case (Num(a), Num(b)) => operator match {
    case "+" => Num(a + b)
    case "-" => Num(a - b)
    case _ => throw new SimpleException(s"Unsupported operator: $operator")
  }
  case _ => throw new SimpleException("Both arguments must be Num types")
}

def interp(fwae: Expr): Expr = fwae match {
  case Num(n) => fwae
  case Add(l, r) => numAdd(interp(l), interp(r))
  case Sub(l, r) => numSub(interp(l), interp(r))
  case With(i, v, e) => interp(subst(e, i, (interp (v))))
  case Id(s) => throw new SimpleException(s"Free identifier: $s")
  case Fun(p, b) => fwae
  case App(ftn, arg) =>
    val evaluatedFtn = interp(ftn)
    evaluatedFtn match {
    case Fun(param, body) => interp(subst(body, param, interp(arg)))
    case _ => throw new SimpleException("Expected a function")
  }
}

@main def hello(): Unit =
  // println("Hello World main")
  // println(Parser.parse("{+ 1 2}"))
  // println(Parser.parse("{{fun {x} {+ x x}} 10}"))
  // println(Parser.parse("{{fun {x} {+ x 1}} 10}"))
  // assert(Parser.parse("{fun {x} {+ x 1}}") == Fun(Symbol("x"),Add(Id(Symbol("x")),Num(1))))
  // assert(Parser.parse("{{fun {x} {+ x x}} 10}") == App(Fun(Symbol("x"),Add(Id(Symbol("x")),Id(Symbol("x")))),Num(10)))
  // assert(Parser.parse("{fun {x} {+ x 1}}") == Fun(Symbol("x"),Add(Id(Symbol("x")),Num(1))))
  println(parse("{{fun {x} {+ x 1}} 10}"))
  // assert(Parser.parse("{{fun {x} {+ x 1}} 10}") == App(Fun(Symbol("x"),Add(Id(Symbol("x")),Num(1))),Num(10)))
  // println(interp(Parser.parse("{with {x 3} {fun {x} {+ x y}}}")))
  println(interp(parse("{with {x 3} {+ x x}}")))

  






  // println(Parser.parse("{with {x {+ 5 5}} {+ x x}}"))
  // println(interp(Parser.parse("{with {x 5} x}"), List[FunDef](), MtSub))
  // println(interp(Parser.parse("{with {x {+ 5 5}} {+ x x}}"), List[FunDef](), MtSub))
  // println(interp(Parser.parse("{with {x 5} {+ x {with {y 5} y} } }"), List[FunDef](), MtSub))
  // println(interp(Parser.parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](FDParser.parseFD("{deffun {fn a} {+ a a}}"), FDParser.parseFD("{deffun {fn a} {+ a a}}")), MtSub))
  // println(interp(Parser.parse("{with {x 1} {an {with {y 10} {+ y x}}}}"), List[FunDef](FDParser.parseFD("{deffun {fn a} {+ a a}}"), FDParser.parseFD("{deffun {an a} {- a a}}")), MtSub))
  // assert(interp(Parser.parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](FDParser.parseFD("{deffun {fn a} {+ a a}}")), MtSub) == 22)
  // println(interp(Parser.parse("{with {x 4} {f 1}}"), List[FunDef](FDParser.parseFD("{deffun {f x} {+ x 3}}")), MtSub))
  // println(interp(Parser.parse("{with {x 4} {f 1}}"), List[FunDef](FDParser.parseFD("{deffun {f y} {+ x y}}")), MtSub))
  // println(interp(Parser.parse("{with {x 4} {an {with {y 10} {+ y x}}}}"), List[FunDef](FDParser.parseFD("{deffun {fn a} {+ a a}}"), FDParser.parseFD("{deffun {an a} {- a a}}")), MtSub))

  //  println(msg)

def msg = "I was compiled by Scala 3. :)"
