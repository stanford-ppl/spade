package spade.node

import spade._
import spade.params._
import spade.network._

abstract class Top(param:DesignParam)(implicit design:Design) extends Module

case class MeshTop(param:MeshDesignParam)(implicit design:Design) extends Top(param) {
  import param._

  @transient val networks = networkParams.map { param => new MeshNetwork(param) }

  val argBundles = networks.map(_.argBundle)
  val argFringe = {
    Module(ArgFringe(argFringeParam, argBundles))
  }

  val cuBundles = networks.map(_.cuBundles)
  val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    val param = pattern.cuAt(i,j)
    Module(Factory.create(param, cuBundles.map(_(i)(j))))
  }

  val sbBundles = networks.map(_.switchBundle)
  val switchArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    Module(SwitchBox(sbBundles.map(_(i)(j))))
  }
}
