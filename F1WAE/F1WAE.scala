import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class With(name: Id, nameExp: Expr, body: Expr) extends Expr
case class Id(name: String) extends Expr
case class App(ftn: Id, arg: Expr) extends Expr

case class FunDef (funName: Id, argName: Id, body: Expr)

// Custom exception class with no stack trace
class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

//;[contract] parse: sexp -> Expr
//;[purprose] to convert s-expression into Expr


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
            wrap(symbol ~ expr) ^^ { case Id(ftn) ~ arg => App(Id(ftn), arg) }

        def parseAllExpr(str: String): Expr =
            parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
    }

    Parser.parseAllExpr(input)
}

//;[contract] parse-fd: sexp -> FunDef
//;[purpose] to conver s-expression for function definition into FunDef

def parseFD(input: String): FunDef = {

    object FDParser extends RegexParsers {
        def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
        def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(s) }
        def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

        lazy val expr: Parser[Expr] =
            int ^^ { case n => Num(n) } |
            symbol |
            wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
            wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
            wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ { case (Id(name) ~ value) ~ body => With(Id(name), value, body) } |
            wrap(symbol ~ expr) ^^ { case Id(ftn) ~ arg => App(Id(ftn), arg) }

        lazy val fundef: Parser[FunDef] =
            wrap("deffun" ~> wrap(symbol ~ symbol) ~ expr) ^^ {
            case (funName ~ argName) ~ body => FunDef(funName, argName, body)
            }

        def parseAllFD(str: String): FunDef =
            parseAll(fundef, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
    }

    FDParser.parseAllFD(input)
}


def subst(expr: Expr, idtf: Id, value: Int): Expr = expr match {
  case Num(num) => expr
  case Add(lhs, rhs) => Add(subst(lhs, idtf, value), subst(rhs, idtf, value))
  case Sub(lhs, rhs) => Sub(subst(lhs, idtf, value), subst(rhs, idtf, value))
  case With(i, v, e) =>
    With(i, subst(v, idtf, value), if (i == idtf) e else subst(e, idtf, value))
  case Id(s) => if (Id(s) == idtf) Num(value) else expr
  case App(ftn, arg) => App(ftn, subst(arg, idtf, value))
}


def lookupFunDef(name: Id, funDefs: List[FunDef]): FunDef = {
  funDefs match {
    case Nil => throw new SimpleException(s"Unknown function: $name")
    case head :: tail =>
      if (head.funName == name) head
      else lookupFunDef(name, tail)
  }
}

def interp(expr: Expr, funDefs: List[FunDef]): Int = expr match {
  case Num(n) => n
  case Add(l, r) => interp(l, funDefs) + interp(r, funDefs)
  case Sub(l, r) => interp(l, funDefs) - interp(r, funDefs)
  case With(i, v, e) => interp(subst(e, i, interp(v, funDefs)), funDefs)
  case Id(s) => throw new SimpleException(s"Free identifier: $s")
  case App(ftn, arg) =>
    val FunDef(_, argName, body) = lookupFunDef(ftn, funDefs)
    interp(subst(body, argName, interp(arg, funDefs)), funDefs)
}


@main def hello(): Unit =
  println("Hello World main")
   println(parse("{with {x 5} x}"))
    println(parse("{with {x 10} {with {y 17} x}}"))
    println(parse("{with {x 10} {with {y x} y}}"))
    println(parse("{with {x 10} {with {y x} x}}"))
   println(parse("{with {x {+ 5 5}} {+ x x}}"))

     println(parse("{f 1}"))

   println(interp(parse("{with {x 5} x}"), List[FunDef]()))
   println(interp(parse("{with {x {+ 5 5}} {+ x x}}"), List[FunDef]()))
   println(interp(parse("{with {x 5} {+ x {with {y 5} y} } }"), List[FunDef]()))
   println(interp(parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}"), parseFD("{deffun {fn a} {+ a a}}"))))
   println(interp(parse("{with {x 1} {an {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}"), parseFD("{deffun {an a} {- a a}}"))))
   assert(interp(parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}"))) == 22)
   println(msg)
  println(parse("{with {x {- 7 2}} {+ x x}}"))
  println(parse("{+ 1 1}"))
  println(parseFD("{deffun {f x} {+ x 3}}"))
  println(interp(Add(Num(1),(Num(1))), List[FunDef](FunDef(Id("f"),Id("x"),Add(Id("x"),Num(3))))))
  assert(interp(Add(Num(1), Num(1)), List[FunDef](FunDef(Id("f"), Id("x"), Add(Id("x"), Num(3))))) == 2)
  assert(interp(Add(Num(1), Num(1)), List[FunDef]()) == 2)
  println(interp(Add(Num(1), Num(1)), List[FunDef]()))

  println(interp(App(Id("f"), Num(1)), List[FunDef](FunDef(Id("f"), Id("x"), Add(Id("x"), Num(3)))))) 
  assert(interp(App(Id("f"), Num(1)), List[FunDef](FunDef(Id("f"), Id("x"), Add(Id("x"), Num(3))))) == 4) 

  println(parseFD("{deffun {f x} {- 20 {twice x}}}"))
  println(parseFD("{deffun {twice y} {+ y y }}"))
  println(interp(App(Id("f"), Num(10)), List[FunDef](FunDef(Id("f"), Id("x"), Sub(Num(20), App(Id("twice"), Id("x")))), FunDef(Id("twice"), Id("y"), Add(Id("y"), Id("y")))))) 
  assert(interp(App(Id("f"), Num(10)), List[FunDef](FunDef(Id("f"), Id("x"), Sub(Num(20), App(Id("twice"), Id("x")))), FunDef(Id("twice"), Id("y"), Add(Id("y"), Id("y"))))) == 0) 
  println(interp(App(Id("f"), Num(5)), List[FunDef](FunDef(Id("f"), Id("x"), Sub(Num(20), App(Id("twice"), Id("x")))), FunDef(Id("twice"), Id("y"), Add(Id("y"), Id("y"))))))


def msg = "I was compiled by Scala 3. :)"