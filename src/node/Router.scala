package spade
package node

case class Router (
  param:RouterParam, 
  override val nios:List[Bundle[_<:PinType]]
)(implicit design:SpadeDesign) extends Routable(nios)

