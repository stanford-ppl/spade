package spade.network

import spade._
import spade.node._
import spade.params._
import spade.util._

import prism._
import scala.collection.mutable._

case class MeshTop(param:MeshTopParam)(implicit design:Design) extends Top(param) {
  import param._
  import design.spademeta._

  @transient val nodes = ListBuffer[Node]()
  case class Node(param:Parameter, nios:ListBuffer[GridBundle[_<:BundleType]]=ListBuffer.empty, coord:Option[(Int,Int)]=None) {
    nodes += this
  }

  @transient val argFringe = Node(argFringeParam)

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    val param = pattern.cuAt(i,j)
    Node(param, coord=Some((i,j)))
  }

  //@transient val dramAGs = List.tabulate(2, numRows+1) { case (x, y) => Node(dramAGsParam, coord=Some(if (x==0) (-1, y) else (numCols, y))) }
  //@transient val sramAGs = List.tabulate(2, numRows+1) { case (x, y) => if (x==0) (-1, y) else (numCols, y) }
  @transient val mcArray = List.tabulate(2, numRows+1) { case (x, y) => 
    Node(mcParam, coord=Some(if (x==0) (-1, y) else (numCols, y)))
  }

  @transient val sbArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    Node(switchParam, coord=Some((i,j)))
  }

  @transient val networks = networkParams.map { param => new MeshNetwork(param, this) }

  nodes.foreach { case Node(param, nios, coord) => 
    val m = Module(Factory.create(param, nios.toList))
    coord.foreach { coord => indexOf(m) = coord }
  }

}
