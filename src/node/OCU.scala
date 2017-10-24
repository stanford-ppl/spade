package spade.node

import spade._
import spade.util._

import pirc.util._

case class OuterComputeUnitParam (
  cfifoSize:Int = 16,
  sfifoSize:Int = 16,
  numCins:Int = 4,
  numScalarFifos:Int = 5,
  numRegs:Int = 0,
  numCtrs:Int = 6,
  muxSize:Int = 10
) extends ComputeUnitParam() {
  val numVectorFifos:Int = 0
  val numControlFifos:Int = 4
  val numSRAMs:Int = 0
  val numStages:Int = 0
  val ops = Nil 

  val numVouts:Int = 0
  val numSouts:Int = 0
  val numCouts:Int = 4

  val vfifoSize:Int = 0
  val sramSize:Int = 0
  override lazy val numLanes:Int = 1
}

case class OuterComputeUnitConfig (
  override val name:String,
  isSeq:Boolean,
  isMeta:Boolean
) extends ControllerConfig(name)

class OuterComputeUnit(override val param:OuterComputeUnitParam=new OuterComputeUnitParam())(implicit spade:Spade) 
  extends ComputeUnit(param) with Configurable {
  import spademeta._
  import param._

  type CT = OuterComputeUnitConfig
  override val typeStr = "ocu"

  val ctrlBox:OuterCtrlBox = Module(new OuterCtrlBox(CtrlBoxParam()))

}
