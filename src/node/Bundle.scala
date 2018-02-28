package spade.node

import spade._

import prism.node._
import prism.enums._
import prism.util._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

trait Edge extends prism.node.Edge[SpadeNode]() {
  type A = Bundle[_]
}
trait DirectedEdge[E<:Edge] extends prism.node.DirectedEdge[SpadeNode, E] with Edge {
}

class InputEdge[B<:BundleType:ClassTag](val src:Bundle[B])(implicit design:Design) extends prism.node.Input[SpadeNode] with DirectedEdge[OutputEdge[B]] {
  val id = design.nextId
  type E <: OutputEdge[B]
  def connect(p:Bundle[B]):Unit = connect(p.out) 
  def <== (p:Bundle[B]):Unit = connect(p) 
  def <== (ps:List[Bundle[B]]):Unit = ps.foreach { p => connect(p) }
}
class OutputEdge[B<:BundleType:ClassTag](val src:Bundle[B])(implicit design:Design) extends prism.node.Output[SpadeNode] with DirectedEdge[InputEdge[B]] {
  val id = design.nextId
  type E <: InputEdge[B]
}

abstract class Bundle[B<:BundleType:ClassTag](implicit src:Module, design:Design) extends SpadeNode with Atom[SpadeNode] {
  setParent(src)
  val in:InputEdge[B] = new InputEdge(this)
  val out:OutputEdge[B] = new OutputEdge(this) 
}
case class Wire[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Design) extends Bundle[B]
abstract class Port[B<:BundleType:ClassTag](implicit src:Module, design:Design) extends Bundle[B] {
  val external:DirectedEdge[_<:Edge]
  val internal:DirectedEdge[_<:Edge]
  def ic = internal

  type PT <: Port[B]
  def connected:List[PT] = external.connected.map(_.src.asInstanceOf[PT])
  def isConnected:Boolean = external.isConnected
  def isConnectedTo(p:PT) = external.connected.contains(p.external)
  def connect(p:PT):Unit = external.connect(p.external)
  def disconnectFrom(e:PT):Unit = external.disconnectFrom(e.external)
}
case class Input[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Design) extends Port[B] {
  type PT = Output[B]
  override val external:InputEdge[B] = in
  override val internal:OutputEdge[B] = out
  override def ic:OutputEdge[B] = internal
  def <== (p:OutputEdge[B]):Unit = external.connect(p)
  def <== (p:PT):Unit = connect(p)
  def <== (p:Wire[B]):Unit = ic.connect(p.in)
  def <== (ps:List[Bundle[B]]):Unit = ps.foreach { p => 
    p match {
      case p:PT => <==(p)
      case p:Wire[B] => <==(p)
      case p => err(s"$this cannot connect to $p")
    }
  }
  def slice[T<:BundleType:ClassTag](idx:Int)(implicit module:Module, design:Design):Bundle[T] = {
    val sl = Module(Slice[T,B](idx), s"$this.slice($idx)")(module, design)
    this <== sl.out
    sl.in
  }
}
case class Output[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Design) extends Port[B] {
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
case class BroadCast[I<:BundleType:ClassTag, O<:BundleType:ClassTag]()(implicit design:Design) extends Module {
  val in = Input[I](s"in")
  val out = Output[O](s"out")
}

case class Slice[I<:BundleType:ClassTag, O<:BundleType:ClassTag](idx:Int)(implicit design:Design) extends Module {
  val in = Input[I](s"in")
  val out = Output[O](s"out")
}

abstract class NetworkBundle[B<:BundleType:ClassTag]()(implicit design:Design) extends Module {
  def inputs:List[Input[B]]
  def outputs:List[Output[B]]

  val isControl = util.isControl[B]
  val isScalar = util.isScalar[B]
  val isVector = util.isVector[B]
  val asControl = util.asControl(this)
  val asScalar = util.asScalar(this)
  val asVector = util.asVector(this)
}
case class GridBundle[B<:BundleType:ClassTag]()(implicit design:Design) extends NetworkBundle[B] {
  import GridBundle._

  val inMap = Map[String, ListBuffer[Input[B]]]()
  val outMap = Map[String, ListBuffer[Output[B]]]()

  def inputs = eightDirections.flatMap { dir => inMap.getOrElse(dir,Nil).toList } 
  def outputs = eightDirections.flatMap { dir => outMap.getOrElse(dir, Nil).toList }  

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
