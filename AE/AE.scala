import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr

class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

def parse(input: String): Expr = {
  object Parser extends RegexParsers {
    def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

    // commented lines are incorrect
    // equivalent to the racket code before adding (= 3 (length sexp))
    lazy val expr: Parser[Expr] =
      int ^^ { case n => Num(n) } |
      // wrap("+" ~> repsep(expr, " ")) ^^ { case terms => Add(terms) } |
      wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
      // wrap("-" ~> repsep(expr, " ")) ^^ { case terms => Sub(terms) }
      wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) }

    def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new Exception(s"bad syntax: $str"))
  }

  Parser.parseAllExpr(input)
}

def interp(expr: Expr): Int = expr match {
  case Num(n) => n
  case Add(l, r) => interp(l) + interp(r)
  case Sub(l, r) => interp(l) - interp(r)
}

object MainApp extends App {
  assert(parse("3") == Num(3))
  assert(parse("{+ 3 4}") == Add(Num(3), Num(4)))
  // assert(parse("{- 5 1 2}") == "java.lang.Exception: Bad syntax: (- 5 1 2)")
  
  assert(interp(parse("3")) == 3)
  assert(interp(parse("{+ 3 4}")) == 7)
  assert(interp(parse("{+ {- 3 4} 7}")) == 6)
}
