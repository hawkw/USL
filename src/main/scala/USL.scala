import scala.collection.mutable
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

  val DataStack = mutable.Stack[Data]()
  type IStream = List[Data]
  val Definitions = mutable.Map[Lit, Data]()

  def prog: Parser[Obj] = rep(obj|num|strlit|lit) ^^{Obj(_)}

  def num: Parser[Num] = """[0-9]+\.?[0-9]*""".r ^^{s => Num(s.toDouble)}
  def strlit: Parser[Obj] = stringLiteral ^^{ s => Obj(s.toList.reverse.map({c => Lit(c.toString)}))}
  def lit: Parser[Lit] = """[^{}"\s]+""".r ^^{s => Lit(s.trim())}
  def obj: Parser[Obj] = "{" ~> rep(num|strlit|lit|obj) <~ "}" ^^{ l => Obj(l)}


  def evalOne(): Boolean = {
    if(DataStack.isEmpty) return false
    DataStack.top match {
      case Lit("+") => {
        DataStack.pop()
        val a: Num = DataStack.pop.asInstanceOf[Num]
        val b: Num = DataStack.pop.asInstanceOf[Num]
        DataStack.push(Num(a.n + b.n))
        true
      }
      case Lit("-") => {
        DataStack.pop()
        val b = DataStack.pop().asInstanceOf[Num]
        val a = DataStack.pop().asInstanceOf[Num]
        DataStack.push(Num(a.n - b.n))
        true
      }
      case Lit("fl") => {
        DataStack.pop()
        val a = DataStack.pop()
        val b = DataStack.pop()
        DataStack.push(a)
        DataStack.push(b)
        true
      }
      case Lit("lup") => {
        DataStack.pop()
        if(DataStack.top.isInstanceOf[Lit]) {
          val i = DataStack.pop().asInstanceOf[Lit]
          Definitions.get(i) match {
            case Some(g) => DataStack.push(g)
            case None => DataStack.push(i)
          }
          true
        } else if(DataStack.top.isInstanceOf[Obj]) {
          val o = DataStack.pop().asInstanceOf[Obj]
          o.data.reverse.foreach(DataStack.push(_))
          true
        } else {
          true
        }
      }
      case Lit("dup") => {
        DataStack.pop()
        DataStack.push(DataStack.top)
        true
      }
      case Lit("ident?") => {
        DataStack.pop()
        if(DataStack.pop().isInstanceOf[Lit]) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }
        true
      }
      case Lit("obj?") => {
        DataStack.pop()
        if(DataStack.pop().isInstanceOf[Obj]) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }
        true
      }
      case Lit("if") => {
        DataStack.pop()
        val test = DataStack.pop()
        val falsey = DataStack.pop().asInstanceOf[Obj]
        val truthy = DataStack.pop().asInstanceOf[Obj]
        val put = test match {
          case Num(0) => falsey
          case _ => truthy
        }
        put.data.reverse.foreach({
          DataStack.push(_)
        })
        true
      }
      case Lit("undef") => {
        DataStack.pop()
        val l = DataStack.pop().asInstanceOf[Obj]
        l.data.reverse.foreach({
          case s: Lit => Definitions.remove(s)
          case _ =>
        })
        true
      }
      case Lit("print") => {
        DataStack.pop()
        println(DataStack.top)
        true
      }
      case Lit("drop") => {
        DataStack.pop()
        DataStack.pop()
        true
      }
      case Lit("clear") => {
        DataStack.clear()
        false
      }
      case Lit("def") => {
        DataStack.pop()
        val bind = DataStack.pop()
        val name = DataStack.pop().asInstanceOf[Lit]
        Definitions.put(name, bind)
        true
      }
      case _ => false
    }
  }

  def run(s: IStream): Unit = {
    if(s.length == 0) {
      println("> " + DataStack)
      return
    }
    val top = s.head
    if(top.isInstanceOf[Lit] && Definitions.contains(top.asInstanceOf[Lit])) {
      val pack = Definitions.get(top.asInstanceOf[Lit]).get
      pack match {
        case Obj(l) => run(l ::: s.tail)
        case _ => run(Definitions.get(top.asInstanceOf[Lit]).get :: s.tail)
      }
    } else {
      DataStack.push(top)
      while (evalOne()) {}
      if(!DataStack.isEmpty && DataStack.top.isInstanceOf[Lit] && Definitions.contains(DataStack.top.asInstanceOf[Lit])) {
        run(Definitions.get(DataStack.pop().asInstanceOf[Lit]).get :: s.tail)
      } else {
        run(s.tail)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var v = ""
    while(v != "exit") {
      v = Console.in.readLine()
      try {
        run(parseAll(prog,v).get.data)
      } catch {
        case e: Throwable => println(e)
      }
    }
  }
}