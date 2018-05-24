package spade
package param

import spade.node._
import prism.enums._
import prism.node._

trait CUParam extends Parameter {
  // Memory
  val numSrams:Int
  val sramParam:SRAMParam
  val numControlFifos:Int
  val controlFifoParam:FIFOParam
  val numScalarFifos:Int
  val scalarFifoParam:FIFOParam
  val vectorFifoParam:FIFOParam
  val numCtrs:Int
  val simdParam:Option[SIMDParam]

  // -----   Derived parameters
  var cu:CU = _
  def set(cu:CU):Unit = {
    this.cu = cu
    simdParam.foreach{ _.set(cu) }
  }
  lazy val numCins = cbundleOf(cu).fold(0) { _.inputs.size }
  lazy val numSins = sbundleOf(cu).fold(0) { _.inputs.size }
  lazy val numVins = vbundleOf(cu).fold(0) { _.inputs.size }
  lazy val numCouts = cbundleOf(cu).fold(0) { _.outputs.size }
  lazy val numSouts = sbundleOf(cu).fold(0) { _.outputs.size }
  lazy val numVouts = vbundleOf(cu).fold(0) { _.outputs.size }
  lazy val numVectorFifos = numVins
}
case class PCUParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  numCtrs:Int=6,
  simdParam:Option[SIMDParam]=Some(DefaultSIMDParam(numStages=6, vectorized=true, numRegs=16))
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0,0)
}
case class SCUParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  numCtrs:Int=6,
  simdParam:Option[SIMDParam]=Some(DefaultSIMDParam(numStages=6, vectorized=false, numRegs=16))
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0,0)
}
case class PMUParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  sramParam:SRAMParam=SRAMParam(size=256 * 1024 / 4,4), // 256 kB
  numCtrs:Int=6
) extends CUParam {
  val numSrams:Int = 1 
  val simdParam = None
}
case class SramAGParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  numCtrs:Int=6,
  simdParam:Option[SIMDParam]=Some(DefaultSIMDParam(numStages=6, vectorized=true, numRegs=16))
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0,0)
}
case class DramAGParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  numCtrs:Int=6,
  simdParam:Option[SIMDParam]=Some(DefaultSIMDParam(numStages=6, vectorized=false, numRegs=16))
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0,0)
}
