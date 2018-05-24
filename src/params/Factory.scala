package spade
package node

import prism.enums._
import scala.collection.mutable._
import pureconfig._

object Factory extends Logging {
  def create(param:DesignParam)(implicit design:Spade) = {
    SpadeDesign(param)
  }
  def create(param:Parameter)(implicit design:SpadeDesign) = param match {
    case param:StaticMeshTopParam => StaticMeshTop(param)
    case param:DynamicMeshTopParam => DynamicMeshTop(param)
    case param:StaticCMeshTopParam => StaticCMeshTop(param)
    case param:AsicTopParam => AsicTop(param)
  }
  def create(param:Parameter, nios:List[Bundle[_<:PinType]])(implicit design:SpadeDesign) = param match {
    case param:PCUParam => PCU(param, nios)
    case param:PMUParam => PMU(param, nios)
    case param:SCUParam => SCU(param, nios)
    case param:SramAGParam => SramAG(param, nios)
    case param:DramAGParam => DramAG(param, nios)
    case param:ArgFringeParam => ArgFringe(param, nios)
    case param:MCParam => MC(param, nios)
    case param:SwitchParam => SwitchBox(param, nios)
    case param:RouterParam => Router(param, nios)
  }

}
