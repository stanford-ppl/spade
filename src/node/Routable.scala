package spade.node

import spade.params._
import prism.enums._

abstract class Routable(nios:List[NetworkBundle[_]])(implicit design:Design) extends Module {
  nios.foreach { _.setParent(this) }

  val cio = nios.flatMap { _.asControl }.headOption
  val sio = nios.flatMap { _.asScalar }.headOption
  val vio = nios.flatMap { _.asVector }.headOption
}


