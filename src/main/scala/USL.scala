import scala.util.parsing.combinator.JavaTokenParsers
import scala.math.Numeric

/**
 * Created by hawk on 11/5/14.
 */
abstract class Data
case class Obj(data: List[Data]) extends Data {
  override def toString = s"{ ${data.mkString(" ")} }"
}
case class Num(n: Double) extends Data {
  override def toString = n.toString
}
case class Lit(s: String) extends Data{
  override def toString = s"'$s'"
}

object USL extends JavaTokenParsers {

  def prog: Parser[Obj] = rep(obj|num|strlit|lit) ^^ {case l => Obj(for {n <- l if n.isInstanceOf[Data]} yield {n.asInstanceOf[Data]}) }

  def num: Parser[Num] = """[0-9]+\.?[0-9]*""".r ^^{ case n => Num(n.toDouble)}
  def strlit: Parser[Obj] = stringLiteral ^^{ case s => Obj(s.toList.reverse.map({c => Lit(c.toString)}))}
  def lit: Parser[Lit] = """[^{}"]+[{}]""".r ^^{case s => Lit(s.trim())}
  def obj: Parser[Obj] = "{" ~> rep(num|strlit|lit|obj) <~ "}" ^^{ case l => Obj(l)}

  def main(args: Array[String]): Unit = {
    var i = ""
    while(i != "exit") {
      i = Console.in.readLine()
      println(parseAll(prog,i))
    }
  }
}
