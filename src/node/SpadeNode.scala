package spade.newnode

import spade._
import prism.node._
import scala.language.reflectiveCalls

import scala.collection.mutable.ListBuffer

abstract class SpadeNode(implicit design:Spade) extends Node[SpadeNode] { self =>
  type N = SpadeNode
  type P = Module
  type A = Bundle
}

abstract class Module(implicit design:Spade) extends SpadeNode with SubGraph[SpadeNode] { self =>
}

trait Edge extends prism.node.Edge[SpadeNode]() {
  type A = Bundle
}
trait DirectedEdge[E<:Edge] extends prism.node.DirectedEdge[SpadeNode, E] with Edge

class InputEdge(val src:Bundle)(implicit design:Spade) extends DirectedEdge[OutputEdge]
class OutputEdge(val src:Bundle)(implicit design:Spade) extends DirectedEdge[InputEdge]

abstract class Bundle(implicit src:Module, design:Spade) extends SpadeNode with Atom[SpadeNode] {
  val external:DirectedEdge[_<:Edge]
  val internal:DirectedEdge[_<:Edge]
  def ic = internal

  def connected:List[Bundle] = external.connected.map(_.src)
  def isConnected:Boolean = external.isConnected
  def isConnectedTo(p:Bundle) = external.connected.contains(p.external)
  def connect(p:Bundle):this.type = { external.connect(p.external); this }
  def disconnectFrom(e:Bundle):Unit = external.disconnectFrom(e.external)

  def connect(p:Edge):this.type = { internal.connect(p); this }
}
class Input(implicit src:Module, design:Spade) extends Bundle {
  val external = new InputEdge(this)
  val internal = new OutputEdge(this) 
  override def ic:OutputEdge = internal
}
class Output(implicit src:Module, design:Spade) extends Bundle {
  val external = new OutputEdge(this) 
  val internal = new InputEdge(this) 
  override def ic:InputEdge = internal
}

class SRAM(implicit design:Spade) extends Module {
}
