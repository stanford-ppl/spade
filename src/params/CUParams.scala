package spade.params

import spade.node._

import pirc.enums._
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
  val simdParam:SIMDParam

  // -----   Derived parameters
  var cu:CU = _
  def set(cu:CU):Unit = {
    this.cu = cu
    simdParam.set(cu)
  }
  lazy val numCins = cu.cio.fold(0) { _.inputs.size }
  lazy val numSins = cu.sio.fold(0) { _.inputs.size }
  lazy val numVins = cu.vio.fold(0) { _.inputs.size }
  lazy val numCouts = cu.cio.fold(0) { _.outputs.size }
  lazy val numSouts = cu.sio.fold(0) { _.outputs.size }
  lazy val numVouts = cu.sio.fold(0) { _.outputs.size }
  lazy val numVectorFifos = numVins
}
case class PCUParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  numCtrs:Int=6,
  simdParam:SIMDParam=DefaultSIMDParam(numStages=6, numLanes=16, numRegs=16)
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0)
}
case class SCUParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  numCtrs:Int=6,
  simdParam:SIMDParam=DefaultSIMDParam(numStages=6, numLanes=1, numRegs=16)
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0)
}
case class PMUParam (
  numControlFifos:Int=6,
  numScalarFifos:Int=6,
  controlFifoParam:FIFOParam=FIFOParam(size=4),
  scalarFifoParam:FIFOParam=FIFOParam(size=4),
  vectorFifoParam:FIFOParam=FIFOParam(size=4),
  sramParam:SRAMParam=SRAMParam(size=256),
  numCtrs:Int=6
) extends CUParam {
  val numSrams:Int = 1 
  val simdParam = DefaultSIMDParam(
    numStages=0,
    numLanes=0,
    numRegs=0
  )
}
