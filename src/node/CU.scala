package spade.node

import spade.params._

import scala.reflect._

import scala.collection.mutable._

abstract class CU(val param:CUParam, nios:List[Bundle[_<:PinType]])(implicit design:Design) extends Routable(nios) {
  param.set(this) // Compute derived parameters
  import param._

  /* ------- SUBMODULES --------------*/

  val counters = Modules("ctr", numCtrs, Counter())
  val srams = Modules("sram", numSrams, SRAM(sramParam))
  val controlFifos = Modules("cfifo", numControlFifos, FIFO[Bit](controlFifoParam))
  val scalarFifos  = Modules("sfifo", numScalarFifos, FIFO[Word](scalarFifoParam))
  val vectorFifos  = Modules("vfifo", numVectorFifos, FIFO[Vector](vectorFifoParam))

  val simd = simdParam.map { param => Module(SIMDUnit(param)) }

  // SIMD input connection
  simd.foreach { simd =>
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
  }
  //// Memory Connection
  vio.foreach { vio =>
    (vio.inputs.zip(vectorFifos)).foreach { case (input, fifo) => fifo.writePort <== input.ic }
  }
  sio.foreach { sio =>
    scalarFifos.foreach { fifo => fifo.writePort <== sio.inputs.map(_.ic) }
  }
  cio.foreach { cio =>
    controlFifos.foreach { fifo => fifo.writePort <== cio.inputs.map(_.ic) }
  }
  //TODO SRAM. multiple writer

}

case class PCU(override val param:PCUParam, override val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends CU(param, nios) 
case class PMU(override val param:PMUParam, override val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends CU(param, nios) 
case class SCU(override val param:SCUParam, override val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends CU(param, nios) 
case class SramAG(override val param:SramAGParam, override val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends CU(param, nios) 
case class DramAG(override val param:DramAGParam, override val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends CU(param, nios) 
