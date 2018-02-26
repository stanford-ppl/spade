package spade.newnode

import spade._
import prism.node._
import pirc.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable.ListBuffer

trait Edge extends prism.node.Edge[SpadeNode]() {
  type A = Bundle[_]
}
trait DirectedEdge[E<:Edge] extends prism.node.DirectedEdge[SpadeNode, E] with Edge

class InputEdge(val src:Bundle[_])(implicit design:Spade) extends DirectedEdge[OutputEdge]
class OutputEdge(val src:Bundle[_])(implicit design:Spade) extends DirectedEdge[InputEdge]

abstract class Bundle[B<:BundleType:ClassTag](implicit src:Module, design:Spade) extends SpadeNode with Atom[SpadeNode] {
  val ctg = implicitly[ClassTag[B]]
  val external:DirectedEdge[_<:Edge]
  val internal:DirectedEdge[_<:Edge]
  def ic = internal

  def connected:List[Bundle[B]] = external.connected.map(_.src.asInstanceOf[Bundle[B]])
  def isConnected:Boolean = external.isConnected
  def isConnectedTo(p:Bundle[_]) = external.connected.contains(p.external)
  def connect(p:Bundle[B]):this.type = { external.connect(p.external); this }
  def disconnectFrom(e:Bundle[_]):Unit = external.disconnectFrom(e.external)
}
case class Input[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Spade) extends Bundle[B] {
  val external = new InputEdge(this)
  val internal = new OutputEdge(this) 
  override def ic:OutputEdge = internal

  def <== (p:Bundle[B]):this.type = connect(p) 
  def <== (ps:List[Bundle[B]]):this.type = { ps.foreach { p => connect(p) }; this }
  def slice[T<:BundleType:ClassTag](idx:Int)(implicit module:Module, design:Spade):Bundle[T] = {
    val sl = Module(Slice[T,B](idx), s"$this.slice($idx)")(module, design)
    this <== sl.out
    sl.in
  }
}
case class Output[B<:BundleType:ClassTag](name:String)(implicit src:Module, design:Spade) extends Bundle[B] {
  val external = new OutputEdge(this) 
  val internal = new InputEdge(this) 
  override def ic:InputEdge = internal
  def broadCast[T<:BundleType:ClassTag](implicit module:Module, design:Spade):Bundle[T] = {
    val bc = Module(BroadCast[B,T](), s"$this.broadcast")(module, design)
    bc.in <== this
    bc.out
  }
  def slice[T<:BundleType:ClassTag](idx:Int)(implicit module:Module, design:Spade):Bundle[T] = {
    val sl = Module(Slice[B,T](idx), s"$this.slice($idx)")(module, design)
    sl.in <== this
    sl.out
  }
}

case class BroadCast[I<:BundleType:ClassTag, O<:BundleType:ClassTag]()(implicit design:Spade) extends Module {
  val in = Input[I](s"in")
  val out = Output[O](s"out")
}

case class Slice[I<:BundleType:ClassTag, O<:BundleType:ClassTag](idx:Int)(implicit design:Spade) extends Module {
  val in = Input[I](s"in")
  val out = Output[O](s"out")
}

sealed trait BundleType
trait Bit extends BundleType
trait Word extends BundleType
trait Vector extends BundleType

abstract class NetworkBundle[B<:BundleType:ClassTag]()(implicit spade:Spade) extends Module {
  val ctg = implicitly[ClassTag[B]]
  def inputs:List[Input[B]]
  def outputs:List[Output[B]]
  val isControl = newnode.isControl[B]
  val isScalar = newnode.isScalar[B]
  val isVector = newnode.isVector[B]
}
