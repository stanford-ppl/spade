package spade.node

import spade._
import spade.params._
import prism.node._
import prism.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

abstract class SpadeNode(val id:Int) extends Node[SpadeNode] { self =>
  def this()(implicit design:Design) = this(design.nextId)
  type N = SpadeNode
  type P = Module
  type A = Bundle[_]
}

abstract class Module(id:Int) extends SpadeNode(id) with SubGraph[SpadeNode] {
  def this()(implicit design:Design) = this(design.nextId)
  implicit val module:Module = this
}
object Module {
  def apply[M<:Module](module:M, name:String)(implicit parent:Module, design:Design):M = {
    module.setParent(parent)
  }
  def apply[M<:Module](module:M)(implicit parent:Module, design:Design):M = {
    module.setParent(parent)
  }
}

object Modules {
  def apply[M<:Module](name:String, modules:List[M])(implicit parent:Module, design:Design):List[M] = {
    modules.map(m => Module(m, name))
  }
  def apply[M<:Module](name:String,num:Int,lambda: => M)(implicit parent:Module, design:Design):List[M] = {
    List.tabulate(num)( i => Module(lambda, name) )
  }
}

abstract class OnChipMem(param:OnChipMemParam)(implicit design:Design) extends Module {
  val dequeueEnable = Input[Bit](s"deqEn")
  val enqueueEnable = Input[Bit](s"deqEn")
  val notEmpty = Input[Bit](s"notEmpty")
  val notFull = Input[Bit](s"notFull")
  val counter = Module(BufferCounter(), "counter")

  counter.inc <== enqueueEnable.ic
  counter.dec <== dequeueEnable.ic
  notFull <== counter.notFull
  notEmpty <== counter.notEmpty
}
case class BufferCounter()(implicit sapde:Design) extends Module {
  val inc = Input[Bit](s"inc")
  val dec = Input[Bit](s"dec")
  val count = Output[Word](s"count")
  val notFull = Output[Bit](s"notFull")
  val notEmpty = Output[Bit](s"notEmpty")
}

case class SRAM(param:SRAMParam)(implicit design:Design) extends OnChipMem(param) {
  val writePort = Input[Vector](s"writePort")
  val writeAddr = Input[Vector](s"writeAddr")
  val readAddr = Input[Vector](s"readAddr")
  val readPort = Output[Vector](s"readPort")
}

case class FIFO[B<:BundleType:ClassTag](param:FIFOParam)(implicit design:Design) extends OnChipMem(param) {
  val writePort = Input[B](s"writePort")
  val readPort = Output[B](s"readPort")
}

case class Reg()(implicit design:Design) extends OnChipMem(new OnChipMemParam { val size = 1 }) {
  val writePort = Input[Word](s"writePort")
  val readPort = Output[Word](s"readPort")
}

case class Counter()(implicit design:Design) extends Module {
  val min = Input[Word](s"min")
  val max = Input[Word](s"max")
  val step = Input[Word](s"step")
  val en = Input[Bit](s"en")
  val out = Output[Vector](s"out")
  val done = Input[Bit](s"done")
}

case class UpDownCounter()(implicit sapde:Design) extends Module {
  val inc = Input[Vector](s"writePort")
  val dec = Input[Vector](s"writeAddr")
  val count = Input[Vector](s"readAddr")
}

case class PipeReg(param:PipeRegParam)(implicit design:Design) extends Module {
  val in = Input[Vector](s"in")
  val out = Output[Vector](s"out")
}
case class FuncUnit(param:FuncUnitParam)(implicit design:Design) extends Module {
  val operands = List.tabulate(param.numOperands)(i => Input[Vector](s"operand[$i]"))
  val out = Output[Vector]("out")
}

case class Stage(param:StageParam)(implicit design:Design) extends Module {
  val funcUnit = Module(FuncUnit(param.funcUnitParam), "funcUnit")
  val pipeRegs = Modules("pipeReg",param.pipeRegParams.map { param => PipeReg(param) })
}

case class SIMDUnit(param:SIMDParam)(implicit design:Design) extends Module {
  val stages = param.stageParams.map { param => Module(Stage(param), "stage") }

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

abstract class CU(val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends Routable(nios) {
  param.set(this) // Compute derived parameters
  import param._

  /* ------- SUBMODULES --------------*/

  val counters = Modules("ctr", numCtrs, Counter())
  val srams = Modules("sram", numSrams, SRAM(sramParam))
  val controlFifos = Modules("cfifo", numControlFifos, FIFO[Bit](controlFifoParam))
  val scalarFifos  = Modules("sfifo", numScalarFifos, FIFO[Word](scalarFifoParam))
  val vectorFifos  = Modules("vfifo", numVectorFifos, FIFO[Vector](vectorFifoParam))

  val simd = Module(SIMDUnit(simdParam))

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
    (vio.inputs.zip(vectorFifos)).foreach { case (input, fifo) => fifo.writePort <== input.ic }
  }
  sio.foreach { sio =>
    scalarFifos.foreach { fifo => fifo.writePort <== sio.inputs }
  }
  cio.foreach { cio =>
    controlFifos.foreach { fifo => fifo.writePort <== cio.inputs }
  }
  //TODO SRAM. multiple writer

}

case class PCU(override val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends CU(param, nios)

case class SCU(override val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends CU(param, nios)

case class PMU(override val param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends CU(param, nios)

case class ArgFringe(param:ArgFringeParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends Routable(nios) {
}

case class SwitchBox(nios:List[NetworkBundle[_]])(implicit design:Design) extends Routable(nios)

abstract class Routable(nios:List[NetworkBundle[_]])(implicit design:Design) extends Module {
  nios.foreach { _.setParent(this) }

  val cio = nios.flatMap { _.asControl }.headOption
  val sio = nios.flatMap { _.asScalar }.headOption
  val vio = nios.flatMap { _.asVector }.headOption
}

