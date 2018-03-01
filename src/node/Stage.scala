package spade.node

import spade.params._
import prism.enums._

case class PipeReg(param:PipeRegParam)(implicit design:Design) extends Module {
  val in = Input[Vector](s"in")
  val out = Output[Vector](s"out")
}
case class FuncUnit(param:FuncUnitParam)(implicit design:Design) extends Module {
  val operands = Inputs[Vector](s"operand", param.numOperands)
  val out = Output[Vector]("out")
}

case class Stage(param:StageParam)(implicit design:Design) extends Module {
  val funcUnit = Module(FuncUnit(param.funcUnitParam), "funcUnit")
  val pipeRegs = Modules("pipeReg",param.pipeRegParams.map { param => PipeReg(param) })
}
