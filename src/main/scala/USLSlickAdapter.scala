import org.newdawn.slick._

import scala.collection.mutable

/**
 * USLSlickAdapter:
 * Fancy name for a dumb class that wraps Slick2D in such a way that USL can use it without
 * fear.
 */
class USLSlickAdapter {
  class AdapterFakeGame(val title: String) extends BasicGame(title) {
    abstract class Instruction
    case class IColor(r: Float, g: Float, b: Float) extends Instruction
    case class ILine(x1: Float, x2:Float, x3:Float, x4:Float) extends Instruction
    case class IBG(r:Float, g:Float, b:Float) extends Instruction

    val IQueue = mutable.Queue[Instruction]()
    var pbuf: Image = null

    def color(r: Float, g: Float, b: Float): Unit = {
      IQueue.synchronized({
        IQueue.enqueue(IColor(r,g,b))
      })
    }

    def line(x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
      IQueue.synchronized({
        IQueue.enqueue(ILine(x1,y1,x2,y2))
      })
    }

    def background(r: Float, g: Float, b: Float): Unit = {
      IQueue.synchronized({
        IQueue.enqueue(IBG(r,g,b))
      })
    }

    override def init(container: GameContainer): Unit = {
      pbuf = new Image(600,400)
    }

    def run(i: Instruction, gfx: Graphics) = i match {
      case IColor(r,g,b) => gfx.setColor(new Color(r,g,b))
      case IBG(r,g,b) =>
        gfx.clear()
        val pcolor = gfx.getColor
        gfx.setColor(new Color(r,g,b))
        gfx.fillRect(0,0,600,400)
        gfx.setColor(pcolor)
      case ILine(x1,y1,x2,y2) => gfx.drawLine(x1,y1,x2,y2)
      case _ => ???
    }

    override def update(container: GameContainer, delta: Int): Unit = {
    }

    override def render(container: GameContainer, gfx: Graphics): Unit = {
      val graphics = pbuf.getGraphics
      IQueue.synchronized({
        while (IQueue.nonEmpty) {
          run(IQueue.dequeue(), graphics)
        }
      })
      gfx.drawImage(pbuf, 0, 0)
    }
  }
  var container: AppGameContainer = null
  var fakegame: AdapterFakeGame = null

  def init(): Unit = {
    if (container == null) {
      fakegame = new AdapterFakeGame("USL Graphics")
      container = new AppGameContainer(fakegame, 600, 400, false)
      container.setAlwaysRender(true)
      new Thread(new Runnable {def run() {container.start()}}).start()
    }
  }

  def color(r: Float, g:Float, b:Float): Unit = {
    init()
    fakegame.color(r,g,b)
  }
  def line(x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
    init()
    fakegame.line(x1,y1,x2,y2)
  }
  def background(r: Float, g: Float, b: Float): Unit = {
    init()
    fakegame.background(r,g,b)
  }
}
