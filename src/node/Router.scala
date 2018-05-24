package spade
package node
import param._

case class Router (
  param:RouterParam, 
  override val bundles:List[Bundle[_<:PinType]]
)(implicit design:SpadeDesign) extends Routable(bundles)

