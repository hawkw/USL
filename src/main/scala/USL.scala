import java.io.File

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.math.Numeric

/**
 * Ported from Napkin by hawk on 11/5/14.
 * Significant refactory/updates by Xyzzy on 11/12/14
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
  var gfx: USLSlickAdapter = null

  def prog: Parser[Obj] = rep(obj|num|strlit|lit) ^^{Obj(_)}

  def num: Parser[Num] = """[0-9]+\.?[0-9]*""".r ^^{s => Num(s.toDouble)}
  def strlit: Parser[Obj] = stringLiteral ^^{ s => Obj(s.toList.tail.reverse.tail.map({c => Lit(c.toString)}))}
  def lit: Parser[Lit] = """[^{}"\s]+""".r ^^{s => Lit(s.trim())}
  def obj: Parser[Obj] = "{" ~> rep(num|strlit|lit|obj) <~ "}" ^^{ l => Obj(l)}


  def evalOne(param: Data): Unit = {
    param match {
      case Lit("+") =>
        val a: Num = DataStack.pop().asInstanceOf[Num]
        val b: Num = DataStack.pop().asInstanceOf[Num]
        DataStack.push(Num(a.n + b.n))

      case Lit("-") =>
        val b = DataStack.pop().asInstanceOf[Num]
        val a = DataStack.pop().asInstanceOf[Num]
        DataStack.push(Num(a.n - b.n))

      case Lit("fl") =>
        val a = DataStack.pop()
        val b = DataStack.pop()
        DataStack.push(a)
        DataStack.push(b)

        /** Execpack: Unpacks an object and places all of its symbols on the instruction stack to be executed next. */
      case Lit("execpack") =>
        DataStack.pop().asInstanceOf[Obj].data.reverse.foreach(IStack.push)

        /** Yank: Pop() but for objects at the top of the DataStack */
      case Lit("yank") =>
        val a = DataStack.pop().asInstanceOf[Obj]
        DataStack.push(Obj(a.data.tail))
        DataStack.push(a.data.head)

      case Lit("smush") =>
        val a = DataStack.pop()
        val b = DataStack.pop().asInstanceOf[Obj]
        DataStack.push(Obj(a :: b.data))

      case Lit("lup") =>
        DataStack.pop() match {
          case i: Lit => DataStack.push(Definitions.getOrElse(i, i))
          case o: Obj => o.data.reverse.foreach(DataStack.push)
          case l => DataStack.push(l)
        }

      case Lit("setcol") =>
        gfx.color(DataStack.pop().asInstanceOf[Num].n.toFloat, DataStack.pop().asInstanceOf[Num].n.toFloat, DataStack.pop().asInstanceOf[Num].n.toFloat)
        //eventually we will want to flip on user-defined basis, I think?

      case Lit("line") =>
        gfx.line(DataStack.pop().asInstanceOf[Num].n.toFloat,DataStack.pop().asInstanceOf[Num].n.toFloat,DataStack.pop().asInstanceOf[Num].n.toFloat,DataStack.pop().asInstanceOf[Num].n.toFloat)

      case Lit("bg") =>
        gfx.background(DataStack.pop().asInstanceOf[Num].n.toFloat,DataStack.pop().asInstanceOf[Num].n.toFloat,DataStack.pop().asInstanceOf[Num].n.toFloat)

      case Lit("dup") => DataStack.push(DataStack.top)

      case Lit("eq?") =>
        if(DataStack.pop() == DataStack.pop()) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }

      case Lit("ident?") =>
        if(DataStack.pop().isInstanceOf[Lit]) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }

      case Lit("obj?") =>
        if(DataStack.pop().isInstanceOf[Obj]) {
          DataStack.push(Num(1))
        } else {
          DataStack.push(Num(0))
        }

      case Lit("if") =>
        val test = DataStack.pop()
        val falsey = DataStack.pop().asInstanceOf[Obj]
        val truthy = DataStack.pop().asInstanceOf[Obj]
        val put = test match {
          case Num(0) => falsey
          case _ => truthy
        }
        put.data.reverse.foreach(IStack.push)

      case Lit("undef") =>
        val l = DataStack.pop().asInstanceOf[Obj]
        l.data.reverse.foreach({
          case s: Lit => Definitions.remove(s)
          case _ =>
        })
      case Lit("print") => println(DataStack.top)

      case Lit("drop") => DataStack.pop()

      case Lit("clear") => DataStack.clear()

      case Lit("def") =>
        val bind = DataStack.pop()
        val name = DataStack.pop().asInstanceOf[Lit]
        Definitions.put(name, bind)

      case _ => DataStack.push(param)
    }
  }

  def run(): Unit = {
    while(IStack.length > 0) {
      IStack.pop() match {
        case q: Lit if Definitions.contains(q) => Definitions.get(q).get match {
          case Obj(l) => l.reverse.foreach(IStack.push)
          case l => IStack.push(l)
        }
        case l => evalOne(l)
      }
      /*
      if (top.isInstanceOf[Lit] && Definitions.contains(top.asInstanceOf[Lit])) {
        val pack = Definitions.get(top.asInstanceOf[Lit]).get
        pack match {
          case Obj(l) => l.reverse.foreach(IStack.push)
          case _ => IStack.push(pack)
        }
      } else {
        evalOne(top)
      }*/
    }
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("java.library.path", "dist/natives")
    System.setProperty("org.lwjgl.librarypath", new File("dist/natives").getAbsolutePath())

    gfx = new USLSlickAdapter
    var v = ""
    while(v != "exit") {
      println("> " + DataStack)
      v = Console.in.readLine()
      try {
        parseAll(prog,v).get.data.reverse.foreach(IStack.push)
        run()
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }
}