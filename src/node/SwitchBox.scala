package spade.node

import spade.params._
import prism.enums._

case class SwitchBox(nios:List[NetworkBundle[_]])(implicit design:Design) extends Routable(nios)
