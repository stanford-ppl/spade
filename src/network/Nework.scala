package spade
package node

import scala.collection.mutable

class Network[B<:PinType](
  param:NetworkParam[B], top:Top
)(implicit design:SpadeDesign) {
  implicit val bct = param.bct
  import param._
  import top._

  bundleGroups.foreach { node => node.addBundle(GridBundle[B]()) }

  def tpOf(node:BundleGroup) = node.param match {
    case param:PCUParam => "pcu"
    case param:PMUParam => "pmu"
    case param:SCUParam => "scu"
    case param:ArgFringeParam => "arg"
    case param:MCParam => "mc"
    case param:DramAGParam => "dag"
    case param:RouterParam => "rt"
    case param:SwitchParam => "sb"
  }

}
