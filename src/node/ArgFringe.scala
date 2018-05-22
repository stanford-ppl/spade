package spade
package node

case class ArgFringe(
  param:ArgFringeParam, 
  override val bundles:List[Bundle[_<:PinType]]
  )(implicit design:SpadeDesign) extends Routable(bundles) {
}
