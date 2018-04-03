package spade.node

abstract class Routable(val nios:List[Bundle[_<:PinType]])(implicit design:SpadeDesign) extends Module {
  nios.foreach { _.setParent(this) }

}


