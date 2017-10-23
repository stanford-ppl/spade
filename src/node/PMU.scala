package spade.node

import spade._
import spade.util._

import pirc.enums._
import pirc.util._
import pirc.exceptions._

import scala.collection.mutable.ListBuffer

case class PreloadMemoryComputeParam (
  override val sfifoSize:Int = 16,
  override val vfifoSize:Int = 16,
  override val numRegs:Int = 16,
  override val numCtrs:Int = 8,
  override val muxSize:Int = 10
) extends PatternMemoryUnitParam (
  numVectorFifos = ConfigFactory.plasticineConf.vinPmu,
  numVouts = ConfigFactory.plasticineConf.voutPmu,
  numScalarFifos = ConfigFactory.plasticineConf.sinPmu,
  numSouts = ConfigFactory.plasticineConf.soutPmu,
  numRegs  = ConfigFactory.plasticineConf.regsPmu,
  numStages = ConfigFactory.plasticineConf.rw
) with PreLoadSpadeParam

class PatternMemoryUnitParam(
  val cfifoSize:Int = 16,
  val sfifoSize:Int = 16,
  val vfifoSize:Int = 16,
  val numControlFifos:Int = 4,
  val numCouts:Int = 4,
  val numVectorFifos:Int = 4,
  val numVouts:Int = 4,
  val numScalarFifos:Int = 4,
  val numSouts:Int = 4,
  val numRegs:Int = 16,
  val numStages:Int = 8,
  val numCtrs:Int = 8,
  val muxSize:Int = 10
) extends ComputeUnitParam() {
  val numSRAMs = 1
  val sramSize = 512 * 1024 / 4
  val ops = fixOps ++ otherOps
  override lazy val numLanes = 1
}

class PatternMemoryUnit(override val param:PatternMemoryUnitParam=new PatternMemoryUnitParam())(implicit spade:Spade) 
  extends ComputeUnit(param) {
  override val typeStr = "pmu"
  import spademeta._
  import param._

  lazy val ctrlBox:MemoryCtrlBox = Module(new MemoryCtrlBox(CtrlBoxParam()))

  def sram = srams.head
  override def connect:Unit = {
    super.connect
    vouts.foreach { _.ic <== sram.readPort }
    souts.foreach { _.ic <== (sram.readPort,0) }
  }
}

