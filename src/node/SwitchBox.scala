package spade.node

import spade.params._

case class SwitchBox(param:SwitchParam, override val nios:List[NetworkBundle[_<:BundleType]])(implicit design:Design) extends Routable(nios)
