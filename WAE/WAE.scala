import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class With(name: Id, nameExp: Expr, body: Expr) extends Expr
case class Id(name: String) extends Expr

// Custom exception class with no stack trace
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
        wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => With(Id(name), value, body)}

    def parseAllExpr(input: String): Expr =
        parseAll(expr, input).getOrElse(throw new SimpleException(s"bad syntax: $input"))
    }
    Parser.parseAllExpr(input)
} 

def subst(expr: Expr, idtf: Id, value: Int): Expr = expr match {
    case Num(num) => expr
    case Add(lhs, rhs) => Add(subst(lhs, idtf, value), subst(rhs, idtf, value))
    case Sub(lhs, rhs) => Sub(subst(lhs, idtf, value), subst(rhs, idtf, value))
    case With(i, v, e) =>
        With(i, subst(v, idtf, value), if (i == idtf) e else subst(e, idtf, value))
    case Id(s) => if (Id(s) == idtf) Num(value) else expr
}

def interp(expr: Expr): Int = expr match {
    case Num(n) => n
    case Add(l, r) => interp(l) + interp(r)
    case Sub(l, r) => interp(l) - interp(r)
    case With(i, v, e) => interp(subst(e, i, (interp (v))))
    case Id(s) => throw new SimpleException(s"Free identifier: $s")
}

// main
@main def run(): Unit = {
    assert(subst(Num(5),Id("x"),10) == Num(5)) 

    println(parse("{with {x 5} {+ x x}}"))
    assert(parse("{+ {- 3 4} 7}") == Add(Sub(Num(3),Num(4)),Num(7)))
    assert(parse("{with {x 5} {+ 8 2}}") == With(Id("x"),Num(5),Add(Num(8),Num(2))))
    assert(parse("{with {x 5} {+ x x}}") == With(Id("x"),Num(5),Add(Id("x"),Id("x"))))
    
    println(parse("{with {x 10} {+ 1 x}}"))
    println(interp(parse("{with {x 5} x}")))
    println(interp(parse("{with {x 10} {+ 1 x}}")))
    println(interp(parse("{with {x 10} x}")))
    //println(interp(parse("{with {x 10} y}"))) // Free identifier
    println(interp(parse("{with {x {+ 5 5}} {+ x x}}")))
    println(interp(parse("{with {x 5} x}")))

    assert(interp(parse("{with {x 5} x}")) == 5)
    assert(interp(parse("{with {x 10} {+ 1 x}}")) == 11)
    assert(interp(parse("{with {x 10} x}")) == 10)
    assert(interp(parse("{with {x {+ 5 5}} {+ x x}}")) == 20)
    //assert(interp(parse("{with {x 10} y}")) == "")
    assert({
      try {
        interp(parse("{with {x 10} y}"))
        false
      } catch {
        case e: Exception => 
          e.getMessage == "Free identifier: y"
      }
    })
    
}