package spade
package node

case class SwitchBox (
  param:SwitchParam, 
  override val nios:List[Bundle[_<:PinType]]
)(implicit design:SpadeDesign) extends Routable(nios) {
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
