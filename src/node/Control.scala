package spade.node

import spade.params._
import prism.enums._

case class UpDownCounter()(implicit sapde:Design) extends Module {
  val inc = Input[Vector](s"writePort")
  val dec = Input[Vector](s"writeAddr")
  val count = Input[Vector](s"readAddr")
}

