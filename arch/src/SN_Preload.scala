package arch

import spade.network._
import spade.node._
import spade._

import pirc.enums._

abstract class SN_LD(numRows:Int=2, numCols:Int=2, numArgIns:Int=3, numArgOuts:Int=3, pattern:Pattern=MixAll) extends Spade(
) with PreLoadSpadeParam {
  override def toString = s"SN${numRows}x${numCols}_LD"

  override lazy val topParam = new PreloadTopParam(
    numRows=numRows, 
    numCols=numCols, 
    numArgIns=numArgIns, 
    numArgOuts=numArgOuts, 
    pattern=pattern
  )

  override def pcuAt(i:Int, j:Int) = PreloadPatternComputeParam(numCtrs=12)

  override def pmuAt(i:Int, j:Int) = PreloadMemoryComputeParam(numCtrs=12, numRegs=23)

  override def scuAt(i:Int, j:Int) = PreloadScalarComputeParam()
}


object SN8x8_LD extends SN_LD(numRows=8, numCols=8, numArgIns=12, numArgOuts=5, pattern=Checkerboard) {
  override def scalarNetwork = new ScalarNetwork() {
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 6
  }
}

object SN16x8_LD extends SN_LD(numRows=16, numCols=8, numArgIns=12, numArgOuts=5, pattern=Checkerboard) {
  override def scalarNetwork = new ScalarNetwork() {
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 6
  }
  override def ctrlNetwork = new CtrlNetwork() {

    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 12

    // switch to CU channel width
    channelWidth("pos"->"center", "src"->"sb", "dst"->List("pcu", "mu", "pmu")) = 4

    // CU to Switch channel width
    channelWidth("pos"->"center", "src"->List("pcu", "mu", "pmu"), "dst"->"sb") = 2
      
    // OCU to switch channel width
    channelWidth("pos"->"center", "src"->"ocu", "dst"->"sb") = 4

    // switch to OCU channel width
    channelWidth("pos"->"center", "src"->"sb", "dst"->"ocu") = 4
  }
}

object SN16x13_LD extends SN_LD(numRows=16, numCols=13, numArgIns=15, numArgOuts=5, pattern=Checkerboard) {
  override def scalarNetwork = new ScalarNetwork() {
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 6
  }
}

object SN16x12_LD_HH extends SN_LD(numRows=16, numCols=12, numArgIns=15, numArgOuts=5, pattern=HalfHalf) {
  override def scalarNetwork = new ScalarNetwork() {
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 6
  }
}
