package spade.node

import spade.params._

case class SwitchBox(nios:List[NetworkBundle[_<:BundleType]])(implicit design:Design) extends Routable(nios)
