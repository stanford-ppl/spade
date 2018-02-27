package spade.newnode

import spade._
import prism.node._
import pirc.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

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

object Modules {
  def apply[M<:Module](name:String)(lambda:Spade => List[M])(implicit parent:Module, design:Spade):List[M] = {
    lambda(design).map(m => Module(m, name))
  }
  def apply[M<:Module](name:String,num:Int)(lambda:Spade => M)(implicit parent:Module, design:Spade):List[M] = {
    List.fill(num)(lambda(design)).map(m => Module(m, name))
  }
}

trait OnChipMemParam extends Parameter { 
  val size:Int // Total capacity
}
abstract class OnChipMem(param:OnChipMemParam)(implicit design:Spade) extends Module {
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
case class SRAM(param:SRAMParam)(implicit design:Spade) extends OnChipMem(param) {
  val writePort = Input[Vector](s"writePort")
  val writeAddr = Input[Vector](s"writeAddr")
  val readAddr = Input[Vector](s"readAddr")
  val readPort = Output[Vector](s"readPort")
}

case class FIFOParam(
  size:Int
) extends OnChipMemParam
case class FIFO[B<:BundleType:ClassTag](param:FIFOParam)(implicit design:Spade) extends OnChipMem(param) {
  val writePort = Input[B](s"writePort")
  val readPort = Output[B](s"readPort")
}

case class Reg()(implicit design:Spade) extends OnChipMem(new OnChipMemParam { val size = 1 }) {
  val writePort = Input[Word](s"writePort")
  val readPort = Output[Word](s"readPort")
}

case class Counter()(implicit design:Spade) extends Module {
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
  colors:Set[RegColor]=Set.empty
) extends Parameter {
  def color(c:RegColor) = colors += c
  def is(c:RegColor) = colors.contains(c)
}
case class PipeReg(param:PipeRegParam)(implicit design:Spade) extends Module {
  val in = Input[Vector](s"in")
  val out = Output[Vector](s"out")
}
case class FuncUnitParam(
  numOperands:Int = 3
) extends Parameter
case class FuncUnit(param:FuncUnitParam)(implicit design:Spade) extends Module {
  val operands = block(implicit design => List.tabulate(param.numOperands)(i => Input[Vector](s"operand[$i]")))
  val out = Output[Vector]("out")
}

case class StageParam(
  funcUnitParam:FuncUnitParam=FuncUnitParam(),
  pipeRegParams:List[PipeRegParam],
  reductionIdx:Option[Int] // If the stage can perform reduction, which stage of the reduction can it perform. 
) extends Parameter
case class Stage(param:StageParam)(implicit design:Spade) extends Module {
  val funcUnit = Module(FuncUnit(param.funcUnitParam), "funcUnit")
  val pipeRegs = Modules("pipeReg")(implicit design => param.pipeRegParams.map { param => PipeReg(param) })
}

case class DefaultSIMDParam (
  numStages:Int,
  numLanes:Int,
  numRegs:Int
) extends SIMDParam {
  val numReductionStages = (Math.log(numLanes) / Math.log(2)).toInt
  val numNonReductionStages = numStages - numReductionStages
  val reductionIndices = List.tabulate(numStages){ i =>
    if (i >= numNonReductionStages) Some(i - numNonReductionStages) else None
  }
  def set(cu:CU):Unit = {
    import cu.param._
    pipeRegParams.slice(0, numCtrs).map(_.color(CounterReg))
    if (numReductionStages > 0) {
      pipeRegParams(0).color(ReduceReg) 
      pipeRegParams(1).color(AccumReg) 
    }
    pipeRegParams.takeRight(numScalarFifos).map(_.color(ScalarInReg))
    pipeRegParams.takeRight(numSouts).map(_.color(ScalarOutReg))
    pipeRegParams.takeRight(numVectorFifos).map(_.color(VecInReg))
    pipeRegParams.takeRight(numVouts).map(_.color(VecOutReg))
  }
}

trait SIMDParam extends Parameter {
  val numLanes:Int
  val numRegs:Int
  val reductionIndices:List[Option[Int]]
  def set(cu:CU):Unit
  val pipeRegParams = List.tabulate(numRegs) { ir => PipeRegParam() }
  lazy val stageParams = reductionIndices.map { reductionIdx =>
    StageParam(pipeRegParams=pipeRegParams, reductionIdx=reductionIdx)
  }
}

case class SIMDUnit(param:SIMDParam)(implicit design:Spade) extends Module {
  val stages = Modules("stage")(implicit design => param.stageParams.map { param => Stage(param) })

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
abstract class CU(val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Spade) extends Routable(nios) {
  param.set(this) // Compute derived parameters
  import param._

  /* ------- SUBMODULES --------------*/

  val counters = Modules("ctr", numCtrs)(implicit design => Counter() )
  val srams = Modules("sram", numSrams)(implicit design => SRAM(sramParam) )
  val controlFifos = Modules("cfifo", numControlFifos)(implicit design => FIFO[Bit](controlFifoParam) )
  val scalarFifos  = Modules("sfifo", numScalarFifos )(implicit design => FIFO[Word](scalarFifoParam) )
  val vectorFifos  = Modules("vfifo", numVectorFifos )(implicit design => FIFO[Vector](vectorFifoParam) )

  val simd = Module(SIMDUnit(simdParam))

  block{ implicit design =>
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
    //SIMD output connection
    simd.stages.lastOption.foreach { last =>
      last.pipeRegs.foreach { pipeReg =>
        pipeReg.param.colors.foreach {
          case VecOutReg =>
            vio.foreach { _.outputs.foreach { _.ic <== pipeReg.out } }
          case ScalarOutReg =>
            sio.foreach { _.outputs.foreach { _.ic <== pipeReg.out.slice[Word](0) } }
          case _ =>
        }
      }
    }
    //// Memory Connection
    vio.foreach { vio =>
      (vio.inputs.zip(vectorFifos)).foreach { case (input, fifo) => fifo.writePort <== input }
    }
    sio.foreach { sio =>
      scalarFifos.foreach { fifo => fifo.writePort <== sio.inputs }
    }
    cio.foreach { cio =>
      controlFifos.foreach { fifo => fifo.writePort <== cio.inputs }
    }
    //TODO SRAM. multiple writer
  }

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
case class PCU(override val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Spade) extends CU(param, nios)

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
case class SCU(override val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Spade) extends CU(param, nios)

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
case class PMU(override val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Spade) extends CU(param, nios)

case class ArgFringeParam(
  numArgIns:Int=6,
  numArgOuts:Int=4
) extends Parameter
case class ArgFringe(param:ArgFringeParam, nios:List[NetworkBundle[_]])(implicit design:Spade) extends Routable(nios) {
}

case class SwitchBox(nios:List[NetworkBundle[_]])(implicit design:Spade) extends Routable(nios)

abstract class Routable(nios:List[NetworkBundle[_]])(implicit design:Spade) extends Module {
  nios.foreach { _.setParent(this) }

  val cio = nios.flatMap { _.asControl }.headOption
  val sio = nios.flatMap { _.asScalar }.headOption
  val vio = nios.flatMap { _.asVector }.headOption
}

