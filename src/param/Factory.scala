package spade
package param

import node._
import prism.enums._
import scala.collection.mutable._
import pureconfig._

object Factory extends Logging {
  def create(param:DesignParam)(implicit design:Spade) = {
    SpadeDesign(param)
  }
  def create(param:Parameter)(implicit design:SpadeDesign) = param match {
    case param:StaticGridTopParam => StaticGridTop(param)
    case param:DynamicGridTopParam => DynamicGridTop(param)
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
  def create[B<:PinType](param:NetworkParam[B], top:Top)(implicit design:SpadeDesign) = (param,top) match {
    case (param:DynamicGridNetworkParam[_], top:DynamicGridTop) => DynamicGridNetwork[B](param, top)
    case (param:StaticGridNetworkParam[_], top:StaticGridTop) => StaticGridNetwork[B](param, top)
    case (param, top) => throw PIRException(s"Don't know how to create ($param, $top)")
  }

}
