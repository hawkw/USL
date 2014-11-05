import scala.util.parsing.combinator.RegexParsers
import scala.math.Numeric

/**
 * Created by hawk on 11/5/14.
 */
object USL extends RegexParsers {

  abstract class Data
  case class Obj(data: List[Data]) extends Data
  case class Num(n: Double) extends Data
  case class Lit(s: String) extends Data

  def num: Parser[Num] = """[0-9]+\.?[0-9]*""".r ^^{ case n => Num(n.toDouble)}
  def strlit: Parser[Obj] = "\".+\"".r ^^{ case s => Obj (for { c <- s } yield { Lit(c)}) }
  def lit: Parser[Lit] = """[^\s"{}]+[\s"{}]""".r ^^{case s => Lit(s.trim())}
  def obj: Parser[Obj] = "{" ~> rep(num|strlit|lit|obj) <~ "}" ^^{ case l => Obj(l)}

}
