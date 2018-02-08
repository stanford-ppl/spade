package spade.pass

import spade._
import spade.node._

import pirc._
import pirc.util._

class AreaModel(implicit design: Spade) extends Pass {
  def shouldRun = true 
  import spademeta._

  override def runPass = {
    val numLanes = design.numLanes
    val numRows = design.top.param.numRows
    val numCols = design.top.param.numCols

    // Take a example switch to look at parameter
    val switch = {
      // Take the middle one if possible to be more representative
      val row = if (numRows < 3) design.top.sbArray(0) else design.top.sbArray(1)
      if (numCols < 3) row(0) else row(1)
    }

    dbg(s"numLanes=$numLanes")
    dbg(s"numRows=$numRows")
    dbg(s"numCols=$numCols")
    dbg(s"controlSwitch=${switch.cins.size} x ${switch.couts.size}")
    dbg(s"scalarSwitch=${switch.sins.size} x ${switch.souts.size}")
    dbg(s"vectorSwitch=${switch.vins.size} x ${switch.vouts.size}")
  }

}
