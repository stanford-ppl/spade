package spade.node

trait OnChipMemParam extends Parameter { 
  val size:Int // Total capacity
}
case class SRAMParam(
  size:Int
) extends OnChipMemParam
case class FIFOParam(
  size:Int
) extends OnChipMemParam
case object RegParam extends OnChipMemParam {
  val size = 1
}
