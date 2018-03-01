package spade.node

import spade.params._
import prism.enums._

import scala.reflect._

import scala.collection.mutable._

case class CU(param:CUParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends Routable(nios) {
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
