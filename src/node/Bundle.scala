package spade.node

import spade._

import prism._
import prism.node._
import prism.util._

import scala.language.reflectiveCalls

import scala.collection.mutable._

trait Edge extends prism.node.Edge[SpadeNode]() {
  type A = Bundle[_]
}
abstract class  DirectedEdge[B<:BundleType:ClassTag, E<:Edge:ClassTag] extends prism.node.DirectedEdge[SpadeNode, E] with Edge {
  val bct = implicitly[ClassTag[B]]
}

class InputEdge[B<:BundleType:ClassTag](val src:Bundle[B])(implicit design:Design) extends DirectedEdge[B,OutputEdge[B]] with prism.node.Input[SpadeNode]  {
  val id = design.nextId
  type E <: OutputEdge[B]
  def connect(p:Bundle[B]):Unit = connect(p.out) 
  def <== (p:Bundle[B]):Unit = connect(p) 
  def <== (ps:List[Bundle[B]]):Unit = ps.foreach { p => connect(p) }
}
class OutputEdge[B<:BundleType:ClassTag](val src:Bundle[B])(implicit design:Design) extends DirectedEdge[B,InputEdge[B]] with prism.node.Output[SpadeNode]  {
  val id = design.nextId
  type E <: InputEdge[B]
}

abstract class Bundle[B<:BundleType:ClassTag](implicit val src:Module, design:Design) extends SpadeNode with Atom[SpadeNode] {
  setParent(src)
  val bct = implicitly[ClassTag[B]]
  val in:InputEdge[B] = new InputEdge(this)
  val out:OutputEdge[B] = new OutputEdge(this) 
}
case class Wire[B<:BundleType:ClassTag]()(implicit src:Module, design:Design) extends Bundle[B]
object Wire {
  def apply[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Design):Wire[B] = naming(Wire(), name)
}

abstract class Port[B<:BundleType:ClassTag](implicit src:Module, design:Design) extends Bundle[B] {
  val external:DirectedEdge[B,_<:Edge]
  val internal:DirectedEdge[B,_<:Edge]
  def ic = internal

  type PT <: Port[B]
  def connected:List[PT] = external.connected.map(_.src.asInstanceOf[PT])
  def isConnected:Boolean = external.isConnected
  def isConnectedTo(p:PT) = external.connected.contains(p.external)
  def connect(p:PT):Unit = external.connect(p.external)
  def disconnectFrom(e:PT):Unit = external.disconnectFrom(e.external)
}
case class Input[B<:BundleType:ClassTag]()(implicit src:Module, design:Design) extends Port[B] {
  type PT = Output[B]
  override val external:InputEdge[B] = in
  override val internal:OutputEdge[B] = out
  override def ic:OutputEdge[B] = internal
  def <== (p:OutputEdge[B]):Unit = external.connect(p)
  def <== (p:PT):Unit = connect(p)
  def <== (p:Wire[B]):Unit = ic.connect(p.in)
  def <== (ps:List[Any]):Unit = ps.foreach { p => 
    p match {
      case p:PT => <==(p)
      case p:Wire[B] => <==(p)
      case p:OutputEdge[B] => <==(p)
      case p => err(s"$this cannot connect to $p")
    }
  }
  def slice[T<:BundleType:ClassTag](idx:Int)(implicit module:Module, design:Design):Bundle[T] = {
    val sl = Module(Slice[T,B](idx), s"$this.slice($idx)")(module, design)
    this <== sl.out
    sl.in
  }
}
object Input {
  def apply[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Design):Input[B] = naming(Input(), name)
}
object Inputs {
  def apply[B<:BundleType:ClassTag](name:String, num:Int)(implicit src:Module, design:Design) = {
    indexing(List.fill(num)(Input[B](name)))
  }
}
case class Output[B<:BundleType:ClassTag]()(implicit src:Module, design:Design) extends Port[B] {
  type PT = Input[B]
  override val external:OutputEdge[B] = out 
  override val internal:InputEdge[B] = in
  override def ic:InputEdge[B] = internal
  def broadCast[T<:BundleType:ClassTag](implicit module:Module, design:Design):Bundle[T] = {
    val bc = Module(BroadCast[B,T](), s"$this.broadcast")(module, design)
    bc.in <== this
    bc.out
  }
  def slice[T<:BundleType:ClassTag](idx:Int)(implicit module:Module, design:Design):Bundle[T] = {
    val sl = Module(Slice[B,T](idx), s"$this.slice($idx)")(module, design)
    sl.in <== this
    sl.out
  }
}
object Output {
  def apply[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Design):Output[B] = naming(Output(), name)
}
case class BroadCast[I<:BundleType:ClassTag, O<:BundleType:ClassTag]()(implicit design:Design) extends Module {
  val in = Input[I](s"in")
  val out = Output[O](s"out")
}

case class Slice[I<:BundleType:ClassTag, O<:BundleType:ClassTag](idx:Int)(implicit design:Design) extends Module {
  val in = Input[I](s"in")
  val out = Output[O](s"out")
}

abstract class NetworkBundle[B<:BundleType:ClassTag]()(implicit design:Design) extends Module {
  val bct = implicitly[ClassTag[B]]
  def inputs:List[Input[B]]
  def outputs:List[Output[B]]
}
case class GridBundle[B<:BundleType:ClassTag]()(implicit design:Design) extends NetworkBundle[B] {
  import GridBundle._

  val inMap = Map[String, ListBuffer[Input[B]]]()
  val outMap = Map[String, ListBuffer[Output[B]]]()

  def inAt(dir:String) = inMap.get(dir).map { _.toList }.getOrElse(Nil)
  def outAt(dir:String) = outMap.get(dir).map { _.toList }.getOrElse(Nil)

  def inputs = eightDirections.flatMap { dir => inAt(dir) } 
  def outputs = eightDirections.flatMap { dir => outAt(dir) }  

  def addInAt(dir:String, num:Int)(implicit design:Design):List[Input[B]] = { 
    val ios = List.fill(num)(Input[B]("in"))
    inMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }
  def addOutAt(dir:String, num:Int)(implicit design:Design):List[Output[B]] = {
    val ios = List.fill(num)(Output[B]("out"))
    outMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }

}
object GridBundle {
  val fourDirections = List("W","N","E","S")
  val diagDirections = List("NW","NE","SE","SW")
  val eightDirections = List("W", "NW", "N", "NE", "E", "SE", "S", "SW")
}
