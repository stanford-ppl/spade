package spade.node

import spade._
import spade.util._

import pirc.enums._

import scala.reflect.runtime.universe.{SingleType =>_, _}
import scala.collection.mutable.ListBuffer

abstract class Primitive(implicit spade:Spade) extends Module {
  lazy val prt = collectUp[Routable](this).head
}

case class ConstConfig(value:AnyVal) extends Configuration
class Const[P<:SingleType](tp:P, value:Option[AnyVal])(implicit spade:Spade) 
  extends Simulatable with Configurable {
  type CT = ConstConfig
  override val typeStr = "const"
  val out:Output[P, this.type] = Output(tp.clone, this, s"$this.out")

  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    cfmap.get(this).fold {
      out.v := value
      value.foreach { out.v.default = _ }
    } { config => 
      config.value match {
        case v:Float => out.v := v
        case v:Int => out.v := v
        case v:Boolean => out.v := v
      }
      out.v.default = config.value
    }
  }
}
object Const {
  def apply()(implicit spade:Spade):Const[Word] = new Const(Word(), None)
  def apply(v:Boolean)(implicit spade:Spade):Const[Bit] = new Const(Bit(), Some(v))
  def apply(v:Int)(implicit spade:Spade):Const[Word] = new Const(Word(), Some(v))
  def apply(v:Float)(implicit spade:Spade):Const[Word] = new Const(Word(), Some(v))
}

case class DelayConfig (delay:Int) extends Configuration
class Delay[P<:PortType](tp:P, staticDelay:Option[Int], ts: => String = "delay")(implicit spade:Spade, ev:TypeTag[P]) 
  extends Primitive with Simulatable with Configurable {
  import spademeta._
  type CT = DelayConfig
  override def typeStr = ts 

  val in = Input(tp, this, s"${this}_in(0)")
  val out = Output(tp.clone, this, s"${this}_out")

  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    cfmap.get(this).map { _.delay }.orElse(staticDelay).foreach { delay =>
      out.v := in.vAt(delay) 
    }
  }
}
object Delay {
  def apply[P<:PortType](tp:P,ts: => String)
    (implicit spade:Spade, ev:TypeTag[P]):Delay[P] = new Delay(tp, None, ts)
  def apply[P<:PortType](tp:P, delay:Int, ts: => String)
    (implicit spade:Spade, ev:TypeTag[P]):Delay[P] = new Delay(tp, Some(delay), ts)
  def apply[P<:PortType](tp:P, delay:Int)
    (implicit spade:Spade, ev:TypeTag[P]):Delay[P] = new Delay(tp, Some(delay))
}

abstract class MuxLike[P<:PortType](tp:P, ts: => String)(implicit spade:Spade) extends Primitive with Simulatable {
  import spademeta._

  override lazy val prt:Controller = collectUp[Controller](this).head

  override def typeStr = ts 
  val sel = Input(Word(), this, s"${this}.sel")
  val out = Output(tp.clone, this, s"${this}.out")
  val _inputs = ListBuffer[Input[P, MuxLike[P]]]()
  def inputs = _inputs.toList 
  def addInput = { val i = inputs.size; val in = Input(tp.clone, this, s"${this}.in$i").index(i); _inputs += in; in }
  def addInputs(n:Int):List[Input[P, MuxLike[P]]] = List.fill(n)(addInput) 
  private[spade] def <== (outs:List[Output[P, Module]]):Unit = outs.foreach { out => <==(out) }
  private[spade] def <== (out:Output[P, Module]):Unit = {
    addInput <== out
  }

  override def register(implicit sim:Simulator):Unit = {
    sel.v.default = 0
  }
}

class Mux[P<:PortType](tp:P, ts: => String)(implicit spade:Spade) extends MuxLike(tp, ts) {

  override def register(implicit sim:Simulator):Unit = {
    super.register
    //out.v := inputs(sel.v.toInt).v //TODO: support this
    out.v.set { v => v <<= inputs(sel.v.update.toInt).v.update }
  }
}
object Mux {
  def apply[P<:PortType](ts: => String, tp:P)(implicit spade:Spade):Mux[P] = new Mux(tp, ts)
  def apply[P<:PortType](tp:P)(implicit spade:Spade):Mux[P] = new Mux(tp, "mux")
}

class ValidMux[P<:PortType](tp:P, ts: => String)(implicit spade:Spade) extends MuxLike(tp, ts) {
  val valid = Output(Bit(), this, s"${this}.valid")
  val _valids = ListBuffer[Input[Bit, ValidMux[P]]]()
  def valids = _valids.toList
  override def addInput = {
    val i = inputs.size
    val valid = Input(Bit(), this, s"${this}.valid$i")
    _valids += valid
    super.addInput
  }
  override def register(implicit sim:Simulator):Unit = {
    super.register
    out.v.set { _ <<= inputs(sel.v.update.toInt).v.update }
    valid.v.set { _ <<= valids(sel.v.update.toInt).v.update }
  }
}
object ValidMux {
  def apply[P<:PortType](ts: => String, tp:P)(implicit spade:Spade):ValidMux[P] = new ValidMux(tp, ts)
  def apply[P<:PortType](tp:P)(implicit spade:Spade):ValidMux[P] = new ValidMux(tp, "mux")
}
