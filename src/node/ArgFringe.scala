package spade
package node

case class ArgFringe(
  param:ArgFringeParam, 
  override val nios:List[Bundle[_<:PinType]]
  )(implicit design:SpadeDesign) extends Routable(nios) {
}
