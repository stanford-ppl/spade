package spade.node

import spade.params._
import prism.enums._

case class ArgFringe(param:ArgFringeParam, nios:List[NetworkBundle[_]])(implicit design:Design) extends Routable(nios) {
}
