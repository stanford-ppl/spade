package spade.node

import spade.params._

case class SwitchBox(param:SwitchParam, override val nios:List[Bundle[_<:PinType]])(implicit design:Design) extends Routable(nios) {
  import param._

  connection match {
    case CrossBarSwitchConnection =>
      nios.foreach { bundle =>
        bundle.inputs.foreach { input =>
          bundle.outputs.foreach { output =>
            output.ic <<== input.ic
          }
        }
      }
  }
}
