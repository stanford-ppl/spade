package spade.params

import pirc.enums._
import prism.node._

trait OnChipMemParam extends Parameter { 
  val size:Int // Total capacity
}
case class SRAMParam(
  size:Int
) extends OnChipMemParam
case class FIFOParam(
  size:Int
) extends OnChipMemParam
