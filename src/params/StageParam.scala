package spade.params

import prism.enums._
import prism.node._

import scala.collection.mutable._

case class PipeRegParam(
  colors:Set[RegColor]=Set.empty
) extends Parameter {
  def color(c:RegColor) = colors += c
  def is(c:RegColor) = colors.contains(c)
}
case class FuncUnitParam(
  numOperands:Int = 3
) extends Parameter

case class StageParam(
  funcUnitParam:FuncUnitParam=FuncUnitParam(),
  pipeRegParams:List[PipeRegParam],
  reductionIdx:Option[Int] // If the stage can perform reduction, which stage of the reduction can it perform. 
) extends Parameter
