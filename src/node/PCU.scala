package spade.node

import spade._
import spade.util._

import pirc.util._

case class PreloadPatternComputeParam (
  override val cfifoSize:Int = 16,
  override val sfifoSize:Int = 16,
  override val vfifoSize:Int = 16,
  override val numCtrs:Int = 8,
  override val muxSize:Int = 10
) extends PatternComputeUnitParam (
  numVectorFifos = ConfigFactory.plasticineConf.vinPcu,
  numVouts = ConfigFactory.plasticineConf.voutPcu,
  numScalarFifos = ConfigFactory.plasticineConf.sinPcu,
  numSouts = ConfigFactory.plasticineConf.soutPcu,
  numRegs  = ConfigFactory.plasticineConf.regsPcu,
  numStages = ConfigFactory.plasticineConf.comp
) with PreLoadSpadeParam

case class SRAMAddrGenParam (
  override val sfifoSize:Int = 16,
  override val vfifoSize:Int = 16,
  override val numVectorFifos:Int = 4,
  override val numVouts:Int = 2,
  override val numScalarFifos:Int = 4,
  override val numSouts:Int = 2,
  override val numRegs:Int = 5,
  override val numStages:Int = 4,
  override val numCtrs:Int = 4,
  override val muxSize:Int = 3
) extends PatternComputeUnitParam (
  reduction = false
) with PreLoadSpadeParam

class PatternComputeUnitParam (
  val cfifoSize:Int = 16,
  val sfifoSize:Int = 16,
  val vfifoSize:Int = 16,
  val numVectorFifos:Int = 4,
  val numVouts:Int = 4,
  val numScalarFifos:Int = 4,
  val numSouts:Int = 4,
  val numControlFifos:Int = 2,
  val numCouts:Int = 4,
  val numRegs:Int = 16,
  val numStages:Int = 8,
  val numCtrs:Int = 8,
  val muxSize:Int = 10,
  override val reduction:Boolean = true
) extends ComputeUnitParam() {
  val numSRAMs:Int = 0
  val sramSize:Int = 0
  val ops = pirc.enums.ops
}

class PatternComputeUnit(override val param:PatternComputeUnitParam=new PatternComputeUnitParam())(implicit spade:Spade) 
  extends ComputeUnit(param) {
  import param._
  override val typeStr = "pcu"

  lazy val ctrlBox:InnerCtrlBox = Module(new InnerCtrlBox(CtrlBoxParam()))

}
