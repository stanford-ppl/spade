package spade
package node

import scala.collection.mutable

class DynamicMeshNetwork[B<:PinType](param:DynamicMeshNetworkParam[B], top:DynamicMeshTop)(implicit design:SpadeDesign) {
  implicit val bct = param.bct
  import param._
  import top._

  bundleGroups.foreach { node => node.addBundle(GridBundle[B]()) }

  def tpOf(node:BundleGroup) = node.param match {
    case param:PCUParam => "pcu"
    case param:PMUParam => "pmu"
    case param:SCUParam => "scu"
    case param:RouterParam => "rt"
    case param:ArgFringeParam => "arg"
    case param:MCParam => "mc"
    case param:DramAGParam => "dag"
  }

  def connect(src:BundleGroup, dst:BundleGroup):Unit = {
    val cw = channelWidth("src"->tpOf(src), "dst"->tpOf(dst))
    val key = Seq("src"->tpOf(src), "dst"->tpOf(dst))
    val outs = src.bundle[B].addOuts(cw)
    val ins = dst.bundle[B].addIns(cw)
    outs.zip(ins).foreach { case (o, i) => i <== o }
  }

  def connectTerminalWithSwitch(terminal:BundleGroup) = {
    val (x,y) = terminal.coord.get
    val rt = rtArray(x)(y)
    connect(terminal, rt)
    connect(rt, terminal)
  }

  def connectTerminalArrayWithSwitch(array:List[List[BundleGroup]]) = {
    array.foreach { col => 
      col.foreach { terminal => connectTerminalWithSwitch(terminal) }
    }
  }

  /** ----- Central Array Connection ----- **/
  connectTerminalArrayWithSwitch(cuArray)

  /** ----- Fringe Connection ----- **/
  dagArray.map { dagArray => 
    connectTerminalArrayWithSwitch(dagArray)
    dagArray.zipWithIndex.foreach { case (col, i) =>
      col.zipWithIndex.foreach { case (dag, j) =>
        val mc = mcArray(i)(j)
        connect(mc, dag)
        connect(dag, mc)
      }
    }
  }
  connectTerminalArrayWithSwitch(mcArray)
  connectTerminalWithSwitch(argFringe)

}
