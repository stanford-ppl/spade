package spade.newnode

import spade._
import prism.node._
import pirc.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable.ListBuffer

abstract class SpadeNode(implicit design:Spade) extends Node[SpadeNode] { self =>
  type N = SpadeNode
  type P = Module
  type A = Bundle[_]
}

abstract class Module(implicit design:Spade) extends SpadeNode with SubGraph[SpadeNode] {
  implicit val module:Module = this
}
object Module {
  def apply[M<:Module](module:M, name:String)(implicit parent:Module, design:Spade):M = {
    module.setParent(parent)
  }
  def apply[M<:Module](module:M)(implicit parent:Module, design:Spade):M = {
    module.setParent(parent)
  }
}

trait OnChipMemParam { 
  val size:Int // Total capacity
}
abstract class OnChipMem(param:OnChipMemParam)(implicit spade:Spade) extends Module {
  val dequeueEnable = Input[Bit](s"deqEn")
  val enqueueEnable = Input[Bit](s"deqEn")
  val notEmpty = Input[Bit](s"notEmpty")
  val notFull = Input[Bit](s"notFull")
  val counter = Module(BufferCounter(), "counter")

  counter.inc <== enqueueEnable
  counter.dec <== dequeueEnable 
  notFull <== counter.notFull
  notEmpty <== counter.notEmpty
}
case class BufferCounter()(implicit sapde:Spade) extends Module {
  val inc = Input[Bit](s"inc")
  val dec = Input[Bit](s"dec")
  val count = Output[Word](s"count")
  val notFull = Output[Bit](s"notFull")
  val notEmpty = Output[Bit](s"notEmpty")
}

case class SRAMParam(
  size:Int
) extends OnChipMemParam
case class SRAM(param:SRAMParam)(implicit spade:Spade) extends OnChipMem(param) {
  val writePort = Input[Vector](s"writePort")
  val writeAddr = Input[Vector](s"writeAddr")
  val readAddr = Input[Vector](s"readAddr")
  val readPort = Output[Vector](s"readPort")
}

case class FIFOParam(
  size:Int
) extends OnChipMemParam
case class FIFO[B<:BundleType:ClassTag](param:FIFOParam)(implicit spade:Spade) extends OnChipMem(param) {
  val writePort = Input[B](s"writePort")
  val readPort = Output[B](s"readPort")
}

case class Reg()(implicit spade:Spade) extends OnChipMem(new OnChipMemParam { val size = 1 }) {
  val writePort = Input[Word](s"writePort")
  val readPort = Output[Word](s"readPort")
}

case class Counter()(implicit spade:Spade) extends Module {
  val min = Input[Word](s"min")
  val max = Input[Word](s"max")
  val step = Input[Word](s"step")
  val en = Input[Bit](s"en")
  val out = Output[Vector](s"out")
  val done = Input[Bit](s"done")
}

case class UpDownCounter()(implicit sapde:Spade) extends Module {
  val inc = Input[Vector](s"writePort")
  val dec = Input[Vector](s"writeAddr")
  val count = Input[Vector](s"readAddr")
}

case class PipeRegParam(
  colors:List[RegColor]
)
case class PipeReg(param:PipeRegParam)(implicit spade:Spade) extends Module {
  val in = Input[Vector](s"in")
  val out = Output[Vector](s"out")
}
case class FuncUnitParam(
  numOperands:Int = 3
)
case class FuncUnit(param:FuncUnitParam)(implicit spade:Spade) extends Module {
  val operands = List.tabulate(param.numOperands)(i => Input[Vector](s"operand[$i]"))
  val out = Output[Vector]("out")
}

case class StageParam(
  funcUnitParam:FuncUnitParam=FuncUnitParam(),
  pipeRegParams:List[PipeRegParam],
  reductionIdx:Option[Int] // If the stage can perform reduction, which stage of the reduction can it perform. 
)
case class Stage(param:StageParam)(implicit spade:Spade) extends Module {
  val funcUnit = Module(FuncUnit(param.funcUnitParam), "funcUnit")
  val pipeRegs = param.pipeRegParams.zipWithIndex.map { case (param, i) => Module(PipeReg(param), s"pipeReg[$i]") }
}

case class SIMDParam (
  numStages:Int,
  numLanes:Int,
  pipeRegParams:List[PipeRegParam]
) {
  val numReductionStages = (Math.log(numStages) / Math.log(2)).toInt
  val numNonReductionStages = numStages - numReductionStages
  val stageParams = List.tabulate(numStages){ i =>
    val reductionIdx = if (i >= numNonReductionStages) Some(i - numNonReductionStages) else None
    StageParam(pipeRegParams=pipeRegParams, reductionIdx=reductionIdx)
  }
}
case class SIMDUnit(param:SIMDParam)(implicit spade:Spade) extends Module {
  val stages = param.stageParams.zipWithIndex.map { case (param, i) => Module(Stage(param), s"stage[$i]") }

  stages.zipWithIndex.foreach { case (stage, i) =>
    // Stage Operands 
    stage.funcUnit.operands.foreach { operand =>
      operand <== stage.pipeRegs.map(_.out)
      if (i != 0) operand <== stages(i-1).pipeRegs.map(_.out)
    }
    // Stage Output 
    stage.pipeRegs.foreach { _.in <== stage.funcUnit.out }
  }

}

trait CUParam {
  // Memory
  val numSrams:Int
  val sramParam:SRAMParam
  val numControlFifos:Int
  val controlFifoParam:FIFOParam
  val numScalarFifos:Int
  val scalarFifoParam:FIFOParam
  val numVectorFifos:Int
  val vectorFifoParam:FIFOParam
  val numCtrs:Int
  val simdParam:SIMDParam
}
abstract class CU(param:CUParam, bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends Routable(bundles) {
  import param._

  val counters = List.tabulate(numCtrs) { i => Module(Counter(), s"ctr[$i]") }
  val srams = List.tabulate(numSrams) { i => Module(SRAM(sramParam), s"sram[$i]") }
  val controlFifos = List.tabulate(numControlFifos) { i => Module(FIFO[Bit](controlFifoParam), s"cfifo[$i]") }
  val scalarFifos = List.tabulate(numScalarFifos) { i => Module(FIFO[Word](scalarFifoParam), s"sfifo[$i]") }
  val vectorFifos = List.tabulate(numVectorFifos) { i => Module(FIFO[Vector](vectorFifoParam), s"vfifo[$i]") }

  val simd = SIMDUnit(simdParam)

  // SIMD input connection
  simd.stages.headOption.foreach { head =>
    head.funcUnit.operands.foreach { operand =>
      operand <== srams.map(_.readPort)
      operand <== vectorFifos.map(_.readPort)
      operand <== scalarFifos.map(_.readPort.broadCast[Vector])
    }
    head.pipeRegs.foreach { pipeReg =>
      pipeReg.param.colors.foreach {
        case VecInReg => pipeReg.in <== vectorFifos.map(_.readPort)
        case ScalarInReg => pipeReg.in <== scalarFifos.map(_.readPort.broadCast[Vector])
        case CounterReg => pipeReg.in <== counters.map(_.out)
        case _ =>
      }
    }
  }

  // SIMD output connection
  simd.stages.lastOption.foreach { last =>
    last.pipeRegs.foreach { pipeReg =>
      pipeReg.param.colors.foreach {
        case VecOutReg => //TODO
        case ScalarOutReg =>  //TODO
        case _ =>
      }
    }
  }

}

case class PCUParam (
  numControlFifos:Int,
  controlFifoParam:FIFOParam,
  numScalarFifos:Int,
  scalarFifoParam:FIFOParam,
  numVectorFifos:Int,
  vectorFifoParam:FIFOParam,
  numCtrs:Int,
  simdParam:SIMDParam
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0)
}
case class PCU(param:CUParam, bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends CU(param, bundles)

case class SCUParam (
  numControlFifos:Int,
  controlFifoParam:FIFOParam,
  numScalarFifos:Int,
  scalarFifoParam:FIFOParam,
  numVectorFifos:Int,
  vectorFifoParam:FIFOParam,
  numCtrs:Int,
  simdParam:SIMDParam
) extends CUParam {
  val numSrams:Int = 0
  val sramParam:SRAMParam = SRAMParam(0)
}
case class SCU(param:CUParam, bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends CU(param, bundles)

case class PMUParam (
  sramParam:SRAMParam,
  numControlFifos:Int,
  controlFifoParam:FIFOParam,
  numScalarFifos:Int,
  scalarFifoParam:FIFOParam,
  numVectorFifos:Int,
  vectorFifoParam:FIFOParam,
  numCtrs:Int,
  simdParam:SIMDParam
) extends CUParam {
  val numSrams:Int = 1 
}
case class PMU(param:CUParam, bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends CU(param, bundles)

case class ArgFringeParam(
  numArgIns:Int,
  numArgOuts:Int
)
case class ArgFringe(param:ArgFringeParam, bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends Routable(bundles) {
}

case class SwitchBox(bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends Routable(bundles)

abstract class Routable(bundles:List[NetworkBundle[_]])(implicit spade:Spade) extends Module {
  bundles.foreach { _.setParent(this) }
  val cbundle = bundles.filter { bundle => bundle.isControl }.headOption
  val sbundle = bundles.filter { bundle => bundle.isScalar }.headOption
  val vbundle = bundles.filter { bundle => bundle.isVector }.headOption
}

