package spade.node

import spade._
import spade.util._

import pirc.enums._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

trait PortType extends Value {
  private var _io:IO[_, Module] = _
  def io = _io
  def io_= (value:IO[_, Module]) = _io = value
  def clone(name:Option[String]):this.type
  def clone(name:String):this.type = clone(Some(name))
  override def clone():this.type = clone(None)
  override def toString = s"${io}.t${typeStr}"
}
trait SingleType extends PortType with SingleValue
/* Three types of pin */
case class Bit()(implicit design:Spade) extends SingleType {
  override val typeStr = "b"
  def clone(name:Option[String]):this.type = {
    val ntp = name match {
      case Some(name) => new Bit() { override def toString = name }
      case None => Bit()
    }
    ntp.io = this.io
    ntp.asInstanceOf[this.type]
  }
}
case class Word(wordWidth:Int)(implicit design:Spade) extends SingleType {
  override val typeStr = "w"
  def clone(name:Option[String]):this.type = {
    val ntp = name match {
      case Some(name) => new Word(wordWidth) { override def toString = name }
      case None => Word(wordWidth)
    }
    ntp.io = this.io
    ntp.asInstanceOf[this.type]
  }
}
object Word {
  def apply()(implicit design:Spade):Word = Word(design.wordWidth)
  def apply(name:String)(implicit design:Spade):Word = {
    new Word(design.wordWidth) { override def toString = name }
  }
}
case class Bus(busWidth:Int, elemTp:PortType)(implicit design:Spade) extends PortType with ListValue {
  override val typeStr = "u"
  override def io_= (io:IO[_, Module]) = {
    super.io_= (io)
    elemTp.io = io
  }
  def clone(name:Option[String]):this.type = {
    val ntp = name match {
      case Some(name) => new Bus(busWidth, elemTp.clone) { override def toString = name }
      case None => Bus(busWidth, elemTp.clone)
    }
    ntp.io = this.io
    ntp.asInstanceOf[this.type]
  }
}
object Bus {
  def apply(elemTp:PortType)(implicit design:Spade):Bus = Bus(design.numLanes, elemTp)
}

/* 
 * An input port of a module that can be recofigured to other's output ports
 * fanIns stores the list of ports the input port can configured to  
 * */
abstract class IO[P<:PortType, +S<:Module](val tp:P, val src:S)(implicit design:Spade) extends Node with Val[P] {
  import spademeta._
  src.addIO(this)
  tp.io = this
  override val typeStr = {
    var s = this match {
      case _:Input[_,_] => s"i"
      case _:Output[_,_] => s"o"
    }
    s += tp.typeStr
    s
  } 
  //override def toString =s"${src}.${typeStr}${spademeta.indexOf.get(this).fold(""){idx=>s"[$idx]"}}"
  def isConnected: Boolean
  def disconnect:Unit
  def canConnect(n:IO[_<:PortType, Module]):Boolean
  def indexOf(n:IO[_<:PortType, Module]):Int

  def isIn:Boolean
  def isOut:Boolean
  def asInput = this.asInstanceOf[Input[P,S]]
  def asOutput = this.asInstanceOf[Output[P,S]]
  def isBus = tp.isInstanceOf[Bus]
  def isWord = tp.isInstanceOf[Word]
  def isBit = tp.isInstanceOf[Bit]
  def asBus:IO[Bus, S]
  def asWord:IO[Word, S]
  def asBit:IO[Bit, S]
  def asGlobal:GlobalIO[P, S]

  def isSrcSlice = src.isInstanceOf[Slice[_,_]]
}

/* Input pin. Can only connects to output of the same level */
class Input[P<:PortType, +S<:Module](tp:P, src:S, sf: Option[()=>String])(implicit design:Spade) extends IO[P, S](tp, src) { 
  import spademeta._
  type O = Output[P, Module]
  // List of connections that can map to
  val _fanIns = ListBuffer[O]()
  def fanIns = _fanIns.toList
  def connect(n:O):Unit = { _fanIns += n; n.connectedTo(this) }
  private[spade] def <==(n:O) = { connect(n) }
  private[spade] def <==(ns:Iterable[O]) = ns.foreach(n => connect(n))
  private[spade] def <==(r:PipeReg):Unit = { this.asBus.connect(r.out) }
  private[spade] def <==(n:Output[Bus, Module], i:Int) = n.slice(i, this)
  private[spade] def <==(ns:Iterable[Output[Bus, Module]], i:Int) = ns.foreach(_.slice(i, this))
  private[spade] def <-- (n:Output[_, Module]) = n.broadcast(this.asBus)
  private[spade] def <-- (ns:Iterable[Output[_, Module]]) = ns.foreach{_.broadcast(this.asBus)}
  def ms = s"${this}=fanIns[${_fanIns.mkString(",")}]"
  def canConnect(n:IO[_<:PortType, Module]):Boolean = {
    _fanIns.contains(n)
    //_fanIns.contains(n) || _fanIns.map(_.src).collect{case s:Slice[_] => s.in; case b:BroadCast[_] => b.in }.exists(_.canConnect(na)
  }
  def indexOf(o:IO[_<:PortType, Module]):Int = {
    _fanIns.map { out =>
      out.src match {
        //case s:Slice[_] if s.in.canConnect(o) => o
        //case b:BroadCast[_] if b.in.canConnect(o) => o
        case _ => out
      }
    }.indexOf(o)
  }
  def isConnected = _fanIns.size!=0
  def disconnect = _fanIns.clear
  def isIn:Boolean = true
  def isOut:Boolean = false
  override def asBus:Input[Bus, S] = this.asInstanceOf[Input[Bus, S]]
  override def asWord:Input[Word, S] = this.asInstanceOf[Input[Word, S]]
  override def asBit:Input[Bit, S] = this.asInstanceOf[Input[Bit, S]]
  override def asGlobal:GlobalInput[P, S] = this.asInstanceOf[GlobalInput[P, S]]
  override def toString():String = sf.fold(super.toString) { sf => sf() }
  private lazy val _sliceMap = Map[Int, Slice[_<:PortType, Bus]]() // Int: element index 
  def slice[E<:PortType](i:Int, out:Output[E,Module]):Slice[E, Bus] = {
    val slice = _sliceMap.getOrElseUpdate(i, Slice(out.tp, this.asBus, i)).asInstanceOf[Slice[E, Bus]]
    slice.in <== out
    slice
  }
  def sliceHead[E<:PortType](out:Output[E,Module]):Slice[E, Bus] = slice(0, out)
  def slice[E<:PortType](i:Int):Slice[E, Bus] = _sliceMap(i).asInstanceOf[Slice[E, Bus]]
  def slices:List[Slice[_<:PortType, Bus]] = _sliceMap.values.toList
  def propogate:Input[P, Module] = {
    src match {
      case slice:Slice[_, _] => 
        assert(slice.out.fanOuts.size==1, s"Cannot propogate $slice with fanOuts>1 fanOuts:${slice.out.fanOuts}")
        slice.out.fanOuts.head.asInstanceOf[Input[P, Module]].propogate
      case broadcast:BroadCast[_] => 
        assert(broadcast.out.fanOuts.size==1, s"Cannot propogate $broadcast with fanOuts>1 fanOuts:${broadcast.out.fanOuts}")
        broadcast.out.fanOuts.head.asInstanceOf[Input[P, Module]].propogate
      case _ => this
    }
  }
}
object Input {
  def apply[P<:PortType, S<:Module](t:P, s:S)(implicit design:Spade):Input[P, S] = new Input[P, S](t, s, None)
  def apply[P<:PortType, S<:Module](t:P, s:S, sf: =>String)(implicit design:Spade):Input[P, S] = 
    new Input[P, S](t,s, Some(sf _))
} 

/* Output pin. Can only connects to input of the same level */
class Output[P<:PortType, +S<:Module](tp:P, src:S, sf: Option[()=>String])(implicit design:Spade) extends IO[P, S](tp, src) { 
  import spademeta._
  type I = Input[P, Module]
  val _fanOuts = ListBuffer[I]()
  def fanOuts = _fanOuts.toList
  def connectedTo(n:I):Unit = _fanOuts += n
  private[spade] def ==>(n:I):Unit = { n.connect(this.asInstanceOf[n.O]) }
  private[spade] def ==>(ns:List[I]):Unit = ns.foreach { n => ==>(n) }
  def mt = s"${this}=fanOuts[${_fanOuts.mkString(",")}]" 
  def canConnect(n:IO[_<:PortType, Module]):Boolean = {
    _fanOuts.contains(n)
    //_fanOuts.contains(n) || _fanOuts.map(_.src).collect{case s:Slice[_] => s.out; case b:BroadCast[_] => b.out}.exists(_.canConnect(n))
  }
  def indexOf(i:IO[_<:PortType, Module]):Int = {
    _fanOuts.map { in =>
      in.src match {
        //case s:Slice[_] if s.out.canConnect(i) => i
        //case b:BroadCast[_] if b.out.canConnect(i) => i 
        case _ => in
      }
    }.indexOf(i)
  }
  def isConnected = _fanOuts.size!=0
  def disconnect = _fanOuts.clear
  def isIn:Boolean = false
  def isOut:Boolean = true
  override def asBus:Output[Bus, S] = this.asInstanceOf[Output[Bus, S]]
  override def asWord:Output[Word, S] = this.asInstanceOf[Output[Word, S]]
  override def asBit:Output[Bit, S] = this.asInstanceOf[Output[Bit, S]]
  override def asGlobal:GlobalOutput[P, S] = this.asInstanceOf[GlobalOutput[P, S]]
  override def toString():String = sf.fold(super.toString) { sf => sf() }

  private lazy val _sliceMap = Map[Int, Slice[Bus, _<:PortType]]() // Int: element index 
  def slice[E<:PortType](i:Int, in:Input[E,Module]):Slice[Bus, E] = {
    val slice = _sliceMap.getOrElseUpdate(i, Slice(in.tp, this.asBus, i)).asInstanceOf[Slice[Bus, E]]
    in <== slice.out
    slice
  }
  def sliceHead[E<:PortType](in:Input[E,Module]):Slice[Bus, E] = slice(0, in)
  def slice[E<:PortType](i:Int):Slice[Bus, E] = _sliceMap(i).asInstanceOf[Slice[Bus, E]]
  def sliceHead[E<:PortType]:Slice[Bus, E] = slice(0)
  def slices:List[Slice[Bus, _<:PortType]] = _sliceMap.values.toList
  private lazy val _broadcastMap = Map[Int, BroadCast[P]]() // Int: input buswidth
  def broadcast(in:Input[Bus, Module]):BroadCast[P] = {
    val bc = _broadcastMap.getOrElseUpdate(in.tp.busWidth, BroadCast(this, in.tp.clone)) 
    in <== bc.out
    bc
  }
  def broadcast(busWidth:Int):BroadCast[P] = _broadcastMap(busWidth)
  def getBroadcast(busWidth:Int):Option[BroadCast[P]] = _broadcastMap.get(busWidth)
  def broadcasts:List[BroadCast[P]] = _broadcastMap.values.toList
  def propogate:Output[P, Module] = src match {
    case slice:Slice[_,_] => 
      assert(slice.in.fanIns.size==1, s"Cannot propogate $slice with fanIns>1 fanOuts:${slice.in.fanIns}")
      slice.in.fanIns.head.asInstanceOf[Output[P, Module]].propogate
    case broadcast:BroadCast[_] => 
      assert(broadcast.in.fanIns.size==1, s"Cannot propogate $broadcast with fanIns>1 fanOuts:${broadcast.in.fanIns}")
      broadcast.in.fanIns.head.asInstanceOf[Output[P, Module]].propogate
    case _ => this
  }
} 
object Output {
  def apply[P<:PortType, S<:Module](t:P, s:S)(implicit design:Spade):Output[P, S] = new Output[P,S](t, s, None)
  def apply[P<:PortType, S<:Module](t:P, s:S, sf: =>String)(implicit design:Spade):Output[P, S] = 
    new Output[P, S](t,s, Some(sf _))
}

trait GlobalIO[P<:PortType, +S<:Module] extends IO[P, S] with Simulatable {
  val ic:IO[P, this.type]
  val valid:IO[Bit, this.type]
  def connectedToSwitch:Boolean
  override def reset(implicit sim:Simulator) = {
    super[Simulatable].reset
  }
  override def check(implicit sim:Simulator) = {
    super[Simulatable].check
  }
}

/* Input pin of network element. Has an innernal output */
class GlobalInput[P<:PortType, +S<:Module](tp:P, src:S, sf: Option[()=>String])(implicit design:Spade)
  extends Input(tp, src, sf) with GlobalIO[P,S] { 
  import spademeta._
  val valid:Output[Bit, this.type] = Output(Bit(), this, s"$this.valid")
  val ic:Output[P, this.type] = Output(tp.clone, this, s"$this.ic")
  override def register(implicit sim:Simulator):Unit = {
    fanInOf(this)(sim.mp).foreach { out => 
      ic := out.asGlobal.ic
      valid := out.asGlobal.valid
    }
    valid.v.default = false
  }
  def connectedToSwitch:Boolean = fanIns.exists { _.src.isInstanceOf[SwitchBox] }
  override def ms = s"${super.ms} ic=$ic"
  override def reset(implicit sim:Simulator) = {
    super[Input].reset
    super[GlobalIO].reset
  }
  override def check(implicit sim:Simulator) = {
    super[Input].check
    super[GlobalIO].check
  }
}
object GlobalInput {
  def apply[P<:PortType, S<:Module](t:P, s:S)(implicit design:Spade):GlobalInput[P, S] = new GlobalInput[P, S](t, s, None)
  def apply[P<:PortType, S<:Module](t:P, s:S, sf: =>String)(implicit design:Spade):GlobalInput[P, S] = 
    new GlobalInput[P, S](t,s, Some(sf _))
} 

/* Output pin. Can only connects to input of the same level */
class GlobalOutput[P<:PortType, +S<:Module](tp:P, src:S, sf: Option[()=>String])(implicit design:Spade) 
  extends Output(tp, src, sf) with GlobalIO[P, S] { 
  import spademeta._
  val valid:Input[Bit, this.type] = Input(Bit(), this, s"$this.valid")
  val ic:Input[P, this.type] = Input(tp.clone, this, s"$this.ic")
  override def register(implicit sim:Simulator):Unit = {
    this := ic
    valid.v.default = false
  }
  def connectedToSwitch:Boolean = fanOuts.exists { _.src.isInstanceOf[SwitchBox] }
  override def mt = s"${super.mt} ic=$ic"
  override def reset(implicit sim:Simulator) = {
    super[Output].reset
    super[GlobalIO].reset
  }
  override def check(implicit sim:Simulator) = {
    super[Output].check
    super[GlobalIO].check
  }
} 
object GlobalOutput {
  def apply[P<:PortType, S<:Module](t:P, s:S)(implicit design:Spade):GlobalOutput[P, S] = new GlobalOutput[P,S](t, s, None)
  def apply[P<:PortType, S<:Module](t:P, s:S, sf: =>String)(implicit design:Spade):GlobalOutput[P, S] = 
    new GlobalOutput[P, S](t,s, Some(sf _))
}

case class Slice[PI<:PortType, PO<:PortType](intp:PI, outtp:PO, i:Int)(implicit design:Spade) extends Simulatable {
  override val typeStr = "slice"
  val in = Input(intp.clone(), this, s"${this}.in")
  val out = Output(outtp.clone(), this, s"${this}.out")
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    (in.v, out.v) match {
      case (in:SingleValue, out:ListValue) =>
        out.value(i) := in 
      case (in:ListValue, out:SingleValue) =>
        out := in.value(i)
      case (in:ListValue, out:ListValue) if in.bus.busWidth==1 =>
        out.value(i) := in.value(0)
      case (in:ListValue, out:ListValue) if out.bus.busWidth==1 =>
        out.value(0) := in.value(i)
    }
  }
}

object Slice {
  def apply[P<:PortType](intp:P, fout:Output[Bus,Module], i:Int)(implicit design:Spade):Slice[Bus, P] = {
    val slice = new Slice(fout.tp, intp, i) {
      override def toString = s"$fout.slice($i)"
    }
    slice.in <== fout
    slice
  }
  def apply[P<:PortType](outtp:P, fin:Input[Bus,Module], i:Int)(implicit design:Spade):Slice[P, Bus] = {
    val slice = new Slice(outtp, fin.tp, i) {
      override def toString = s"$fin.slice($i)"
    }
    fin <== slice.out
    slice
  }
}

case class BroadCast[P<:PortType](bout:Output[P,Module], bintp:Bus)(implicit design:Spade) extends Simulatable {
  override val typeStr = "broadcast"
  override def toString = s"${bintp.io}.broadcast"
  val in:Input[P, this.type] = Input(bout.tp.clone(), this, s"${this}.in")
  val out:Output[Bus, this.type] = Output(bintp.clone(), this, s"${this}.out")
  in <== bout
  override def register(implicit sim:Simulator):Unit = {
    out.v.foreach { case (v, i) => v := in.v }
  }
}

trait GridIO[P <:PortType, +NE<:Routable] extends Node {
  import spademeta._
  private val inMap = Map[String, ListBuffer[Input[P, _]]]()
  private val outMap = Map[String, ListBuffer[Output[P, _]]]()

  def src:NE
  def tp:P
  def inputs(num:Int)(implicit design:Spade, nt:GridNetwork):List[GlobalInput[P, NE]] = 
    List.tabulate(num) { i => val in = GlobalInput(tp, src); networkOf(in) = nt; in }
  def outputs(num:Int)(implicit design:Spade, nt:GridNetwork):List[GlobalOutput[P, NE]] = 
    List.tabulate(num) { i => val out = GlobalOutput(tp, src); networkOf(out) = nt; out }
  def addInAt(dir:String, num:Int)(implicit design:Spade, nt:GridNetwork):List[GlobalInput[P, NE]] = { 
    val ibs = inputs(num)
    inMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ibs
    ibs
  }
  def addOutAt(dir:String, num:Int)(implicit design:Spade, nt:GridNetwork):List[GlobalOutput[P, NE]] = {
    val obs = outputs(num)
    outMap.getOrElseUpdate(dir, ListBuffer.empty) ++= obs
    obs
  }
  def addIOAt(dir:String, num:Int)(implicit design:Spade, nt:GridNetwork):NE = {
    addInAt(dir,num)
    addOutAt(dir,num)
    src
  }
  def addIns(num:Int)(implicit design:Spade, nt:GridNetwork):NE = { 
    addInAt("N", num)
    src
  }
  def addOuts(num:Int)(implicit design:Spade, nt:GridNetwork):NE = {
    addOutAt("N", num)
    src
  }
  def inAt(dir:String):List[GlobalInput[P, NE]] = { 
    inMap.getOrElse(dir, ListBuffer.empty).toList.asInstanceOf[List[GlobalInput[P, NE]]]
  }
  def outAt(dir:String):List[GlobalOutput[P, NE]] = { 
    outMap.getOrElse(dir, ListBuffer.empty).toList.asInstanceOf[List[GlobalOutput[P, NE]]]
  }
  def ins:List[GlobalInput[P, NE]] = GridIO.eightDirections.flatMap { dir => inAt(dir) } 
  def outs:List[GlobalOutput[P, NE]] = GridIO.eightDirections.flatMap { dir => outAt(dir) }  
  def ios:List[IO[P, NE]] = ins ++ outs
  def numIns:Int = inMap.values.map(_.size).sum
  def numOuts:Int = outMap.values.map(_.size).sum
  def io(in:Input[P, Routable]):String = {
    val dirs = inMap.filter{ case (dir, l) => l.contains(in) }
    assert(dirs.size==1)
    val (dir, list) = dirs.head
    s"${dir.toLowerCase}_${list.indexOf(in)}"
  }
  def clearIO:Unit = {
    inMap.clear
    outMap.clear
  }
}
object GridIO {
  def fourDirections = { "W" :: "N" :: "E" :: "S" ::Nil }
  def eightDirections = { "W" :: "NW" :: "N" :: "NE" :: "E" ::  "SE" :: "S" :: "SW" ::Nil }
  def diagDirections = {"NW":: "NE":: "SE":: "SW" :: Nil}
}

case class ControlIO[+N<:Routable](src:N)(implicit design:Spade) extends GridIO[ControlIO.P, N] {
  override def toString = s"${src}.ctrlIO"
  override def tp = Bit()
}
object ControlIO {
  type P = Bit 
}

case class ScalarIO[+N<:Routable](src:N)(implicit design:Spade) extends GridIO[ScalarIO.P, N] {
  override def toString = s"${src}.scalarIO"
  override def tp = Word()
}
object ScalarIO {
  type P = Word
}

case class VectorIO[+N<:Routable](src:N)(implicit design:Spade) extends GridIO[VectorIO.P, N] {
  override def toString = s"${src}.vectorIO"
  override def tp = Bus(design.numLanes, Word())
}
object VectorIO {
  type P = Bus
}
