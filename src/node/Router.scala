package spade
package node

case class Router (
  param:RouterParam, 
  override val bundles:List[Bundle[_<:PinType]]
)(implicit design:SpadeDesign) extends Routable(bundles)

