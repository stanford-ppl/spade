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

  @transient val bundles = ListBuffer[BundleGroup]()

  @transient val argFringe = fringePattern.argBundle

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    centrolPattern.cuAt(i,j)
  }

  @transient val sbArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    centrolPattern.switchAt(i,j)
  }

  //@transient val dramAGs = List.tabulate(2, numRows+1) { case (i, j) => BundleGroup(dramAGsParam, coord=Some(if (i==0) (-1, j) else (numCols, j))) }
  //@transient val sramAGs = List.tabulate(2, numRows+1) { case (i, j) => if (i==0) (-1, j) else (numCols, j) }
  @transient val mcArray = List.tabulate(2, numRows+1) { case (i, j) => 
    fringePattern.mcAt(i,j)
  }

  @transient val networks = networkParams.map { param => new MeshNetwork(param, this) }

  bundles.foreach { case BundleGroup(param, nios, coord) => 
    val m = Module(Factory.create(param, nios.toList))
    coord.foreach { coord => indexOf(m) = coord }
  }

}
