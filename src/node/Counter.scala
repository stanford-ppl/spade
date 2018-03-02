package spade.node

import spade.params._

case class Counter()(implicit design:Design) extends Module {
  val min = Input[Word](s"min")
  val max = Input[Word](s"max")
  val step = Input[Word](s"step")
  val en = Input[Bit](s"en")
  val out = Output[Vector](s"out")
  val done = Input[Bit](s"done")
}

