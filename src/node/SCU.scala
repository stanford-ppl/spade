package spade.node

import spade._
import spade.util._

import pirc.util._
import pirc.enums._

case class PreloadScalarComputeParam(
  override val sfifoSize:Int = 16,
  override val numSouts:Int = 4,
  override val numRegs:Int = 16,
  override val numCtrs:Int = 6,
  override val muxSize:Int = 10
) extends ScalarComputeUnitParam(
  numScalarFifos = ConfigFactory.plasticineConf.sinUcu,
  numStages = ConfigFactory.plasticineConf.stagesUcu
) with PreLoadSpadeParam

class ScalarComputeUnitParam (
  val cfifoSize:Int = 16,
  val sfifoSize:Int = 16,
  val numControlFifos:Int = 3,
  val numCouts:Int = 4,
  val numScalarFifos:Int = 4,
  val numSouts:Int = 4,
  val numRegs:Int = 16,
  val numStages:Int = 6,
  val numCtrs:Int = 6,
  val muxSize:Int = 10
) extends ComputeUnitParam() {
  val numVectorFifos:Int = 0
  val numVouts:Int = 0
  val vfifoSize:Int = 0
  val numSRAMs:Int = 0
  val sramSize:Int = 0
  val ops = pirc.enums.ops
  override lazy val numLanes:Int = 1
}
/* A spetial type of CU used for memory loader/storer */
class ScalarComputeUnit(override val param:ScalarComputeUnitParam=new ScalarComputeUnitParam())(implicit spade:Spade) 
  extends ComputeUnit(param) {
  import param._
  override val typeStr = "scu"
  lazy val ctrlBox:InnerCtrlBox = Module(new InnerCtrlBox(CtrlBoxParam()))

}
