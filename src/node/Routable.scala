package spade
package node
import param._

abstract class Routable(val bundles:List[Bundle[_<:PinType]])(implicit design:SpadeDesign) extends Module {
  bundles.foreach { _.setParent(this) }

}


