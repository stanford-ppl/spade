package spade.node

import spade.params._

case class ArgFringe(param:ArgFringeParam, nios:List[NetworkBundle[_<:BundleType]])(implicit design:Design) extends Routable(nios) {
}
