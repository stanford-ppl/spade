package spade
package node

import param._
import scala.collection.mutable

case class DynamicGridNetwork[B<:PinType](
  param:DynamicGridNetworkParam[B], 
  top:DynamicGridTop
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
  /* ----- CU to CU Connection ----- */
  for (y <- 0 until numRows) {
    for (x <- 0 until numCols) {
      if (x!=numCols-1) connect(cuArray(x)(y), cuArray(x+1)(y)) // (Horizontal)
      if (y!=numRows-1) connect(cuArray(x)(y), cuArray(x)(y+1)) // (Vertical)
    }
  }

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
