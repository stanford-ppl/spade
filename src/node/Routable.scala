package spade.node

import spade.params._

abstract class Routable(val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends Module {
  nios.foreach { _.setParent(this) }

}


