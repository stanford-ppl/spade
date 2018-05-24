package spade
package node

import scala.collection.mutable

class DynamicMeshNetwork[B<:PinType](
  param:DynamicMeshNetworkParam[B], top:DynamicMeshTop
)(implicit design:SpadeDesign) extends Network[B](param, top) {
  import param._
  import top._

  def connect(a:BundleGroup, b:BundleGroup):Unit = {
    a.connect[B](b, channelWidth("src"->tpOf(a), "dst"->tpOf(b)))
    b.connect[B](a, channelWidth("src"->tpOf(b), "dst"->tpOf(a)))
  }

  def connectTerminalWithSwitch(terminal:BundleGroup) = {
    val (x,y) = terminal.coord.get
    val rt = rtArray(x)(y)
    connect(terminal, rt)
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
      }
    }
  }
  connectTerminalArrayWithSwitch(mcArray)
  connectTerminalWithSwitch(argFringe)

}
