import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

// Expr definition
trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class With(name: Id, nameExp: Expr, body: Expr) extends Expr
case class Id(name: String) extends Expr
case class App(ftn: Id, arg: Expr) extends Expr

// FunDef defintion
case class FunDef (funName: Id, argName: Id, body: Expr)

//
trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: Int, saved: DefrdSub) extends DefrdSub

// Custom SimpleException class with no stack trace
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


//; [contract] subst: Expr symbol number -> Expr
// ; (here, symbol is an identifier and number is the value for the identifier)
// ; [purpose] to substitute second argument with third argument in first argument,
// ; as per the rules of substitution; the resulting expression contains
// ; no free instances of the second argument
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

def lookup(name: Id, ds: DefrdSub): Int = ds match {
  case MtSub => throw new SimpleException(s"Free Identifier: $name")
  case ASub(i, v, saved) => if (i == name) v else lookup(name, saved)
}

//;interp: Expr list-of-FuncDef -> number
def interp(expr: Expr, funDefs: List[FunDef], ds: DefrdSub): Int = expr match {
  case Num(n) => n
  case Add(l, r) => interp(l, funDefs, ds) + interp(r, funDefs, ds)
  case Sub(l, r) => interp(l, funDefs, ds) - interp(r, funDefs, ds)
  case With(i, v, e) => interp(e, funDefs, ASub(i, interp(v,funDefs, ds), ds))
  case Id(s) => lookup(Id(s), ds)
  case App(ftn, arg) =>
    val FunDef(_, argName, body) = lookupFunDef(ftn, funDefs)
    // Dynamic Scope
    interp(body, funDefs, ASub(argName, interp(arg, funDefs, ds), ds)) 
    // Static Scope
    // interp(body, funDefs, ASub(argName, interp(arg, funDefs, ds), MtSub)) 
}

@main def run(): Unit =
  println("Hello World main")
  println(parse("{with {x 5} x}"))
  println(parse("{with {x {+ 5 5}} {+ x x}}"))
  println(interp(parse("{with {x 5} x}"), List[FunDef](), MtSub))
  println(interp(parse("{with {x {+ 5 5}} {+ x x}}"), List[FunDef](), MtSub))
  println(interp(parse("{with {x 5} {+ x {with {y 5} y} } }"), List[FunDef](), MtSub))
  println(interp(parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}"), parseFD("{deffun {fn a} {+ a a}}")), MtSub))
  println(interp(parse("{with {x 1} {an {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}"), parseFD("{deffun {an a} {- a a}}")), MtSub))
  assert(interp(parse("{with {x 1} {fn {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}")), MtSub) == 22)
  println(interp(parse("{with {x 4} {f 1}}"), List[FunDef](parseFD("{deffun {f x} {+ x 3}}")), MtSub))
  println(interp(parse("{with {x 4} {f 1}}"), List[FunDef](parseFD("{deffun {f y} {+ x y}}")), MtSub))
  println(interp(parse("{with {x 4} {f 1}}"), List[FunDef](parseFD("{deffun {f y} {+ x y}}")), ASub(Id("x"), 1, MtSub)))
  // println(interp(parse("{with {x 4} {an {with {y 10} {+ y x}}}}"), List[FunDef](parseFD("{deffun {fn a} {+ a a}}"), parseFD("{deffun {an a} {- a a}}")), MtSub))
  println(interp(parse("{with {x 4} {f 1}}"), List[FunDef](parseFD("{deffun {f y} {+ x y}}")), ASub(Id("x"), 1,
  ASub(Id("y"), 4, ASub(Id("x"), 2, MtSub)))))

   println(msg)

def msg = "I was compiled by Scala 3. :)"

