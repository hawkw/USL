import scala.util.parsing.combinator.RegexParsers
import scala.math.Numeric

/**
 * Created by hawk on 11/5/14.
 */
abstract class Data
case class Obj(data: List[Data]) extends Data {
  override def toString = s"{ ${data.mkString(" ")} }"
}
case class Num(n: Double) extends Data
case class Lit(s: String) extends Data

object USL extends RegexParsers {

  def prog: Parser[Obj] = rep(obj|num|strlit|lit|whiteSpace) ^^ {case l => Obj(for {n <- l if n.isInstanceOf[Data]} yield {n.asInstanceOf[Data]}) }

  def num: Parser[Num] = """[0-9]+\.?[0-9]*""".r ^^{ case n => Num(n.toDouble)}
  def strlit: Parser[Obj] = "\"" ~> rep(".") <~ "\"" ^^{ case s => new Obj(s.map(Lit(_))) }
  def lit: Parser[Lit] = """[^\s"{}]+[\s"{}]""".r ^^{case s => Lit(s.trim())}
  def obj: Parser[Obj] = "{" ~> rep(num|strlit|lit|obj) <~ "}" ^^{ case l => Obj(l)}

  def main(args: Array[String]): Unit = {
    var i = ""
    while(i != "exit") {
      i = Console.in.readLine()
      println(parseAll(prog,i))
    }
  }
}
