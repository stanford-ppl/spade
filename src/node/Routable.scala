package spade.node

import spade.params._

abstract class Routable(nios:List[NetworkBundle[_<:BundleType]])(implicit design:Design) extends Module {
  nios.foreach { _.setParent(this) }

  val cio = nios.flatMap(nio => as[Bit, NetworkBundle](nio)).headOption
  val sio = nios.flatMap(nio => as[Word, NetworkBundle](nio)).headOption
  val vio = nios.flatMap(nio => as[Vector, NetworkBundle](nio)).headOption
}


