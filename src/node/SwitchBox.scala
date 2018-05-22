package spade
package node

case class SwitchBox (
  param:SwitchParam, 
  override val bundles:List[Bundle[_<:PinType]]
)(implicit design:SpadeDesign) extends Routable(bundles) {
  import param._

  connection match {
    case CrossBarSwitchConnection =>
      bundles.foreach { bundle =>
        bundle match {
          case bundle:GridBundle[_] => connectCrossBar(bundle)
        }
      }
  }

  def connectCrossBar(bundle:GridBundle[_<:PinType]) = {
    bundle.inputsByNeighbor.foreach { case (inputFrom, inputs) =>
      bundle.outputsByNeighbor.foreach { case (outputTo, outputs) =>
        if (inputFrom != outputTo) {
          inputs.foreach { input => 
            outputs.foreach { output => output.ic <<== input.ic }
          }
        }
      }
    }
  }
}
