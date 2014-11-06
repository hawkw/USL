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
  val IStack = mutable.Stack[Data]()
  val Definitions = mutable.Map[Lit, Data]()

  def prog: Parser[Obj] = rep(obj|num|strlit|lit) ^^{Obj(_)}

  def num: Parser[Num] = """[0-9]+\.?[0-9]*""".r ^^{s => Num(s.toDouble)}
  def strlit: Parser[Obj] = stringLiteral ^^{ s => Obj(s.toList.reverse.map({c => Lit(c.toString)}))}
  def lit: Parser[Lit] = """[^{}"\s]+""".r ^^{s => Lit(s.trim())}
  def obj: Parser[Obj] = "{" ~> rep(num|strlit|lit|obj) <~ "}" ^^{ l => Obj(l)}


  def evalOne(param: Data): Unit = {
    param match {
      case Lit("+") => {
        val a: Num = DataStack.pop.asInstanceOf[Num]
        val b: Num = DataStack.pop.asInstanceOf[Num]
        DataStack.push(Num(a.n + b.n))
      }
      case Lit("-") => {
        val b = DataStack.pop().asInstanceOf[Num]
        val a = DataStack.pop().asInstanceOf[Num]
        DataStack.push(Num(a.n - b.n))
      }
      case Lit("fl") => {
        val a = DataStack.pop()
        val b = DataStack.pop()
        DataStack.push(a)
        DataStack.push(b)
      }
      case Lit("exec") => {
        IStack.push(DataStack.pop())
      }
      case Lit("yank") => {
        val a = DataStack.pop().asInstanceOf[Obj]
        DataStack.push(Obj(a.data.tail))
        DataStack.push(a.data.head)
      }
      case Lit("smush") => {
        val a = DataStack.pop()
        val b = DataStack.pop().asInstanceOf[Obj]
        DataStack.push(Obj(a :: b.data))
      }
      case Lit("lup") => {
        if(DataStack.top.isInstanceOf[Lit]) {
          val i = DataStack.pop().asInstanceOf[Lit]
          Definitions.get(i) match {
            case Some(g) => DataStack.push(g)
            case None => DataStack.push(i)
          }
        } else if(DataStack.top.isInstanceOf[Obj]) {
          val o = DataStack.pop().asInstanceOf[Obj]
          o.data.reverse.foreach(DataStack.push(_))
        }
      }
      case Lit("dup") => {
        DataStack.push(DataStack.top)
      }
      case Lit("ident?") => {
        if(DataStack.pop().isInstanceOf[Lit]) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }
      }
      case Lit("obj?") => {
        if(DataStack.pop().isInstanceOf[Obj]) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }
      }
      case Lit("if") => {
        val test = DataStack.pop()
        val falsey = DataStack.pop().asInstanceOf[Obj]
        val truthy = DataStack.pop().asInstanceOf[Obj]
        val put = test match {
          case Num(0) => falsey
          case _ => truthy
        }
        put.data.reverse.foreach({
          IStack.push(_)
        })
      }
      case Lit("undef") => {
        val l = DataStack.pop().asInstanceOf[Obj]
        l.data.reverse.foreach({
          case s: Lit => Definitions.remove(s)
          case _ =>
        })
      }
      case Lit("print") => {
        println(DataStack.top)
      }
      case Lit("drop") => {
        DataStack.pop()
      }
      case Lit("clear") => {
        DataStack.clear()
      }
      case Lit("def") => {
        val bind = DataStack.pop()
        val name = DataStack.pop().asInstanceOf[Lit]
        Definitions.put(name, bind)
      }
      case _ => DataStack.push(param)
    }
  }

  def run(): Unit = {
    while(IStack.length > 0) {
      val top = IStack.pop()
      if (top.isInstanceOf[Lit] && Definitions.contains(top.asInstanceOf[Lit])) {
        val pack = Definitions.get(top.asInstanceOf[Lit]).get
        pack match {
          case Obj(l) => l.reverse.foreach(IStack.push(_))
          case _ => IStack.push(pack)
        }
      } else {
        evalOne(top)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var v = ""
    while(v != "exit") {
      println("> " + DataStack)
      v = Console.in.readLine()
      try {
        parseAll(prog,v).get.data.reverse.foreach(IStack.push(_))
        run()
      } catch {
        case e: Throwable => println(e)
      }
    }
  }
}