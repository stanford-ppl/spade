package spade.node

import spade.params._

case class ArgFringe(
  param:ArgFringeParam, 
  override val nios:List[Bundle[_<:PinType]]
  )(implicit design:Design) extends Routable(nios) {
}
