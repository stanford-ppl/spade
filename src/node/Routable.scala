package spade.node

import spade.params._

abstract class Routable(val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends Module {
  nios.foreach { _.setParent(this) }

  val cio = nios.flatMap(nio => as[Bit, Bundle](nio)).headOption
  val sio = nios.flatMap(nio => as[Word, Bundle](nio)).headOption
  val vio = nios.flatMap(nio => as[Vector, Bundle](nio)).headOption
}


