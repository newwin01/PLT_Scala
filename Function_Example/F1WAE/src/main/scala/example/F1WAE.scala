import scala.util.parsing.combinator._

trait F1WAE
case class Num(n: Int) extends F1WAE
case class Add(lhs: F1WAE, rhs: F1WAE) extends F1WAE
case class Sub(lhs: F1WAE, rhs: F1WAE) extends F1WAE
case class With(name: Symbol, nameExp: F1WAE, body: F1WAE) extends F1WAE
case class Id(name: Symbol) extends F1WAE
case class App(ftn: Symbol, arg: F1WAE) extends F1WAE

case class FunDef (funName: Symbol, argName: Symbol, body: F1WAE)

object Parser extends RegexParsers {
  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(Symbol(s)) }
  def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

  lazy val expr: Parser[F1WAE] =
    int ^^ { case n => Num(n) } |
    symbol |
    wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
    wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
    wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => With(name, value, body) } |
    wrap(symbol ~ expr) ^^ { case Id(ftn) ~ arg => App(ftn, arg) }

  def parse(str: String): F1WAE =
    parseAll(expr, str).getOrElse(throw new Exception(s"bad syntax: $str"))
}

object FDParser extends RegexParsers {
  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(Symbol(s)) }
  def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

  lazy val expr: Parser[F1WAE] =
    int ^^ { case n => Num(n) } |
    symbol |
    wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
    wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
    wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => With(name, value, body) } |
    wrap(symbol ~ expr) ^^ { case Id(ftn) ~ arg => App(ftn, arg) }

  lazy val fundef: Parser[FunDef] =
    wrap("deffun" ~> wrap(symbol ~ symbol) ~ expr) ^^ {
      case (Id(funName) ~ Id(argName)) ~ body => FunDef(funName, argName, body)
    }

  def parseFunDef(str: String): FunDef =
    parseAll(fundef, str).getOrElse(throw new Exception(s"bad syntax: $str"))
}

object Substituter {
  def subst(expr: F1WAE, idtf: Symbol, value: Int): F1WAE = expr match {
    case Num(num) => expr
    case Add(lhs, rhs) => Add(subst(lhs, idtf, value), subst(rhs, idtf, value))
    case Sub(lhs, rhs) => Sub(subst(lhs, idtf, value), subst(rhs, idtf, value))
    case With(i, v, e) =>
      With(i, subst(v, idtf, value), if (i == idtf) e else subst(e, idtf, value))
    case Id(s) => if (s == idtf) Num(value) else expr
    case App(ftn, arg) => App(ftn, subst(arg, idtf, value))
  }
}

object Interpreter {
  
  def lookupFunDef(name: Symbol, funDefs: List[FunDef]): FunDef =
    funDefs.find(_.funName == name).getOrElse(throw new Exception(s"Unknown function: $name"))

  def interp(wae: F1WAE, funDefs: List[FunDef] = List()): Int = wae match {
    case Num(n) => n
    case Add(l, r) => interp(l, funDefs) + interp(r, funDefs)
    case Sub(l, r) => interp(l, funDefs) - interp(r, funDefs)
    case With(i, v, e) => interp(Substituter.subst(e, i, interp(v, funDefs)), funDefs)
    case Id(s) => throw new Exception(s"Free identifier: $s")
    case App(ftn, arg) =>
      val FunDef(_, argName, body) = lookupFunDef(ftn, funDefs)
      interp(Substituter.subst(body, argName, interp(arg, funDefs)), funDefs)
  }
}

object Main{

  def main(args: Array[String]): Unit = {
    println("Hello World main")
    println(Interpreter.interp(Parser.parse("{with {x 5} x}"), List[FunDef]()))
    println(Interpreter.interp(Parser.parse("{with {x {+ 5 5}} {+ x x}}"), List[FunDef]()))
    println(Interpreter.interp(Parser.parse("{with {x 5} {+ x {with {y 5} y} } }"), List[FunDef]()))
    println(Interpreter.interp(Parser.parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](FDParser.parseFunDef("{deffun {fn a} {+ a a}}"))))

  }
}